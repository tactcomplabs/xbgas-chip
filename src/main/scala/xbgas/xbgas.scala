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
    extends LazyRoCC(opcodes, nPTWPorts = 0) {
  override lazy val module = new XbgasAccelModuleImp(this)
  override val atlNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(
          TLMasterParameters.v1(
            name = "XbgasRoCC",
            sourceId = IdRange(0, 32)
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
  val buf1valid = RegInit(VecInit(Seq.fill(queueWidth)(false.B)))
  val buf12 = DecoupledCommandDecoder(p) // floating valid

  tl_out.a.valid := buf0.valid && ~buf1valid.reduce(_&&_)
  val get = edgesOut
    .Get(
      fromSource = buf0.bits.rd,
      toAddress = buf0.bits.baseAddr,
      lgSize = buf0.bits.size
    )
    ._2
  val put = edgesOut
    .Put(
      fromSource = buf0.bits.rd,
      toAddress = buf0.bits.baseAddr,
      lgSize = buf0.bits.size,
      data = buf0.bits.storeData
    )
    ._2
  val putPartial = edgesOut
    .Put(
      fromSource = buf0.bits.rd,
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
    val sel = PriorityEncoder(buf1valid.map(~_).asUInt)
    buf1(sel) := (buf0.deq())
    buf1valid(sel) := true.B
  }

  tl_out.d.ready := buf1valid.reduce(_||_) && buf12.ready
  when(tl_out.d.fire) {
    val sel = PriorityMux(
      for (i <- 0 until queueWidth)
        yield (buf1valid(i) && buf1(i).rd === tl_out.d.bits.source) -> i.U
    )
    buf12.enq(buf1(sel))
    buf1valid(sel) := false.B
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
  io.busy := buf0.valid || buf1valid.reduce(_||_) || io.resp.valid
  io.interrupt := false.B
}

class CommandDecoder(implicit val p: Parameters)
    extends Bundle
    with HasCoreParameters {
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
    this
  }
}

object CommandDecoder {
  def apply(cmd: ReadyValidIO[RoCCCommand], p: Parameters) = {
    val decodedCommand = Wire(new CommandDecoder()(p)).decode(cmd.bits)
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
