package xbgas

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._

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
  val commandParser = Module(new CommandParserModule()(p))
  commandParser.io.state := state
  commandParser.io.cmd <> io.cmd.bits
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
      toAddress = commandParser.io.addr,
      lgSize = commandParser.io.size
    )
    ._2
  val put = edgesOut
    .Put(
      fromSource = 0.U,
      toAddress = commandParser.io.addr,
      data = commandParser.io.wdata,
      lgSize = commandParser.io.size
    )
    ._2
  tl_out.a.bits := Mux(commandParser.io.load, get, put)
  when(tl_out.a.fire) {
    state := s_tl_wait
  }

  // receive data from MMIO
  tl_out.d.ready := (state === s_tl_wait)
  val data = Reg(UInt(xLen.W))
  when(tl_out.d.fire) {
    data := tl_out.d.bits.data
    state := Mux(tl_out.d.bits.opcode === TLMessages.AccessAckData, s_resp, s_idle)
  }

  // respond
  io.resp.valid := (state === s_resp)
  io.resp.bits.data := data
  io.resp.bits.rd := commandParser.io.rd
  when(io.resp.fire) {
    state := s_idle
  }

  val tl_out_a = tl_out.a
  val tl_out_d = tl_out.d
  dontTouch(state)
  dontTouch(data)
  dontTouch(tl_out_a)
  dontTouch(tl_out_d)
}

class CommandParserModule(implicit val p: Parameters)
    extends Module
    with HasCoreParameters {
  val io = IO(new Bundle {
    val size = Output(UInt())
    val load = Output(Bool())
    val rd = Output(UInt(5.W))
    val wdata = Output(UInt(xLen.W))
    val cmd = Input(new RoCCCommand)
    val addr = Output(UInt((2 * xLen).W))
    val state = Input(UInt(3.W))
  })
  dontTouch(io)
  assert(io.cmd.rs3 =/= 0.U)
  val stateIsIdle = (io.state === 0.U)
  val reg_cmd = RegEnable(io.cmd, stateIsIdle)
  val cmd = Mux(stateIsIdle, io.cmd, reg_cmd)

  io.size := VecInit(cmd.inst.xs2, cmd.inst.xs1).asUInt
  io.load := (cmd.inst.opcode =/= "b1111011".U)

  // extended addressing
  io.addr := Cat(cmd.rs3, cmd.rs1)

  // data
  io.wdata := cmd.rs2
  io.rd := cmd.inst.rd
}

class RegFile(n: Int, w: Int) {
  val rf = Mem(n, UInt(w.W))
  private def access(addr: UInt) = rf(~addr(log2Up(n) - 1, 0))
  def read(addr: UInt) = {
    access(addr)
  }
  def write(addr: UInt, data: UInt) = {
    when(addr =/= 0.U) {
      access(addr) := data
    }
  }
}

// // request pte *(wont need to in production, all addr are assumed paddr)
// io.ptw(0).req.valid := (state === s_ptw_req)
// io.ptw(0).req.bits.valid := true.B
// io.ptw(0).req.bits.bits.addr := commandParser.io.vpn
// when(io.ptw(0).req.fire) {
//   state := s_ptw_resp
// }

// // receive pte
// val pte = Reg(new PTE)
// val paddr = Wire(UInt(paddrBits.W))
// paddr := Mux(
//   pte.leaf(),
//   Cat(pte.ppn, commandParser.io.vpo),
//   Cat(commandParser.io.vpn, commandParser.io.vpo)
// )
// when(state === s_ptw_resp && io.ptw(0).resp.valid) {
//   pte := io.ptw(0).resp.bits.pte
//   state := s_tl_req
// }

// io.vaddr := _addr(vaddrBits - 1, 0)
// io.vpn := _addr(vaddrBits - 1, pgIdxBits)
// io.vpo := _addr(pgIdxBits - 1, 0)
