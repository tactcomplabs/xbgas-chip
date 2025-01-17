package xbgas

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy.IdRange

class XbgasAccel(opcodes: OpcodeSet)(implicit p: Parameters)
    extends LazyRoCC(opcodes, nPTWPorts = 0)
    with HasXbgasParams {
  override lazy val module = new XbgasAccelModuleImp(this)
  override val atlNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(
          TLMasterParameters.v1(
            name = "XbgasRoCC",
            sourceId = IdRange(0, queueWidth) //IdRange is half open
          )
        )
      )
    )
  )
}

class XbgasAccelModuleImp(outer: XbgasAccel)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters
    with HasXbgasParams {
  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val buf0 = Queue(CommandDecoder(io.cmd, p), queueWidth, flow = true)
  buf0.nodeq()
  val buf1 = RegInit(VecInit(Seq.fill(queueWidth)(CommandDecoder(p))))
  val buf1valid = RegInit(VecInit(Seq.fill(queueWidth)(false.B)))//source id cant match anything in queue
  val buf12 = DecoupledCommandDecoder(p)

  tl_out.a.valid := buf0.valid && ~buf1valid.reduce(_ && _)
  val sel1 = PriorityEncoder(buf1valid.map(~_).asUInt)
  val get = edgesOut
    .Get(
      fromSource = sel1,
      toAddress = buf0.bits.baseAddr,
      lgSize = buf0.bits.size
    )
    ._2
  val put = edgesOut
    .Put(
      fromSource = sel1,
      toAddress = buf0.bits.baseAddr,
      lgSize = buf0.bits.size,
      data = buf0.bits.storeData
    )
    ._2
  val putPartial = edgesOut
    .Put(
      fromSource = sel1,
      toAddress = buf0.bits.baseAddr,
      lgSize = buf0.bits.size,
      data = buf0.bits.storeData,
      mask = buf0.bits.mask
    )
    ._2
  tl_out.a.bits := PriorityMux(
    Seq(
      buf0.bits.load -> get,
      buf0.bits.partial -> putPartial,
      true.B -> put
    )
  )
  when(tl_out.a.fire) {
    buf1(sel1) := buf0.deq()
    buf1valid(sel1) := true.B
  }

  tl_out.d.ready := buf1valid.reduce(_ || _) && buf12.ready
  when(tl_out.d.fire) {
    val sel2 = PriorityMux(
      for (i <- 0 until queueWidth)
        yield (buf1valid(i) && i.U === tl_out.d.bits.source) -> i.U
    )
    when(buf1(sel2).load) {
      buf12.enq(buf1(sel2))
    }
    buf1valid(sel2) := false.B
    buf12.bits.data := new LoadGen(
      buf12.bits.size,
      ~buf12.bits.unsigned,
      buf12.bits.baseAddr,
      tl_out.d.bits.data,
      false.B,
      coreDataBytes
    ).data
  }

  io.resp <> Queue(buf12, queueWidth, flow = true)
  io.busy := buf0.valid || buf1valid.reduce(_ || _) || io.resp.valid
  io.interrupt := false.B

  dontTouch(tl_out.a)
  dontTouch(tl_out.d)
  dontTouch(buf1)
  dontTouch(buf1valid)
  dontTouch(buf0)
}

class CommandDecoder(implicit val p: Parameters)
    extends Bundle
    with HasCoreParameters
    with HasXbgasParams {
  val size = Bits(2.W)
  val extd = Bool()
  val remoteAddr = Bits(64.W)
  val baseAddr = Bits(64.W)
  val storeData = Bits(64.W)
  val mask = Bits(8.W)
  val unsigned = Bool()
  val partial = Bool()
  val rd = Bits(5.W)
  val load = Bool()
  val data = Bits(coreDataBits.W)
  val sourceId = Bits(log2Up(queueWidth).W)

  def decode(cmd: RoCCCommand) = {
    val cmd_uint = cmd.inst.asUInt
    val storeGen = new StoreGen(size, baseAddr, cmd.rs2, 8)
    size := cmd_uint(13, 12)
    extd := cmd_uint(14, 12) === 7.U
    remoteAddr := cmd.rs3
    baseAddr := cmd.rs1
    storeData := storeGen.data
    mask := storeGen.mask
    unsigned := cmd_uint(14)
    partial := size =/= 3.U
    rd := cmd.inst.rd
    data := DontCare
    sourceId := DontCare
    load := Mux1H(
      Seq(
        XbgasOpcodeSet.integerLoad.matches(cmd_uint(6, 0)) -> true.B,
        XbgasOpcodeSet.integerStore.matches(cmd_uint(6, 0)) -> false.B,
        XbgasOpcodeSet.rawInteger
          .matches(cmd_uint(6, 0)) -> (cmd_uint(31, 25) === "b1010101".U)
      )
    )
    this
  }

  def dontCare() = {
    size := DontCare
    extd := DontCare
    remoteAddr := DontCare
    baseAddr := DontCare
    storeData := DontCare
    mask := DontCare
    unsigned := DontCare
    partial := DontCare
    rd := DontCare
    load := DontCare
    data := DontCare
    sourceId := DontCare
    this
  }
}

object CommandDecoder extends HasXbgasParams {
  private val sourceIdInc = RegInit(0.U(log2Up(queueWidth).W))
  def apply(cmd: ReadyValidIO[RoCCCommand], p: Parameters) = {
    val decodedCommand = Wire(new CommandDecoder()(p)).decode(cmd.bits)
    decodedCommand.sourceId := sourceIdInc
    sourceIdInc := sourceIdInc + 1.U

    val decoupledCommand = Wire(Decoupled(chiselTypeOf(decodedCommand)))
    decoupledCommand.valid := cmd.valid
    decoupledCommand.bits := decodedCommand
    cmd.ready := decoupledCommand.ready
    decoupledCommand
  }

  def apply(p: Parameters) = {
    val emptyCommand = Wire(new CommandDecoder()(p)).dontCare()
    emptyCommand
  }
}

object DecoupledCommandDecoder {
  def apply(p: Parameters) = {
    val emptyCommand = CommandDecoder(p)
    val intermediate = Wire(Decoupled(chiselTypeOf(emptyCommand)))
    intermediate.bits := emptyCommand
    intermediate.valid := false.B
    intermediate.ready := false.B
    intermediate
  }
}
