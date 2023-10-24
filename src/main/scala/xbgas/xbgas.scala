package xbgas

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

class XbgasAccel(opcodes: OpcodeSet)(implicit p: Parameters)
    extends LazyRoCC(opcodes, nPTWPorts = 0) {
  override lazy val module = new XbgasAccelModuleImp3(this)
  override val atlNode = TLClientNode(
    Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("XbgasRoCC"))))
  )
}

class XbgasAccelModuleImp3(outer: XbgasAccel)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val s_idle :: s_tl_req :: s_tl_wait :: s_resp :: Nil =
    Enum(4)
  val state = RegInit(s_idle)
  val cmd =
    Wire(new CommandDecoder()).update(RegEnable(io.cmd.bits, state === s_idle))
  io.busy := (state =/= s_idle)
  io.interrupt := false.B

  // receive cmd
  io.cmd.ready := (state === s_idle)
  when(io.cmd.fire) {
    state := s_tl_req
  }

  // request data from MMIO
  val (tl_out, edgesOut) = outer.atlNode.out(0)
  tl_out.a.valid := (state === s_tl_req)
  val get = edgesOut
    .Get(
      fromSource = 0.U,
      toAddress = cmd.baseAddr,
      lgSize = cmd.size
    )
    ._2
  val put = edgesOut
    .Put(
      fromSource = 0.U,
      toAddress = cmd.baseAddr,
      lgSize = cmd.size,
      data = cmd.storeData
    )
    ._2
  val putPartial = edgesOut
    .Put(
      fromSource = 0.U,
      toAddress = cmd.baseAddr,
      lgSize = cmd.size,
      data = cmd.storeData,
      mask = cmd.mask
    )
    ._2
  tl_out.a.bits := Mux(
    cmd.load,
    get,
    Mux(cmd.partial, putPartial, put)
  )
  when(tl_out.a.fire) {
    state := s_tl_wait
  }

  // receive data from MMIO
  tl_out.d.ready := (state === s_tl_wait)
  val data = Reg(UInt(xLen.W))
  when(tl_out.d.fire) {
    state := Mux(
      tl_out.d.bits.opcode === TLMessages.AccessAckData,
      s_resp,
      s_idle
    )
    data := new LoadGen(
      cmd.size,
      ~cmd.unsigned,
      cmd.baseAddr,
      tl_out.d.bits.data,
      false.B,
      coreDataBytes
    ).data
  }

  // respond
  io.resp.valid := (state === s_resp)
  io.resp.bits.data := data
  io.resp.bits.rd := cmd.rd
  io.resp.bits.extd := cmd.extd
  when(io.resp.fire) {
    state := s_idle
  }

  val tl_out_a = tl_out.a
  val tl_out_d = tl_out.d
}

class CommandDecoder(implicit val p: Parameters) extends Bundle {
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

  def update(cmd: RoCCCommand) = {
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
}
