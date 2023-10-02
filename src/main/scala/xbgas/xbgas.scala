package xbgas

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.rocket.constants.ScalarOpConstants
import freechips.rocketchip.rocket.ImmGen

class XbgasAccel(opcodes: OpcodeSet)(implicit p: Parameters)
    extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new XbgasAccelModuleImp3(this)
  override val atlNode = TLClientNode(
    Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("XbgasRoCC"))))
  )
}

class XbgasAccelModuleImp3(outer: XbgasAccel)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val s_idle :: s_mem_req :: s_tl_req :: s_mem_wait :: s_tl_wait :: s_resp :: Nil =
    Enum(6)
  val state = RegInit(s_idle)
  val commandParser = Module(new CommandParserModule()(p))
  commandParser.io.state := state
  commandParser.io.cmd <> io.cmd.bits
  io.busy := (state =/= s_idle)
  io.interrupt := false.B

  // receive cmd
  io.cmd.ready := (state === s_idle)
  when(io.cmd.fire) {
    state := Mux(commandParser.io.local, s_mem_req, s_tl_req)
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
    state := Mux(commandParser.io.load, s_resp, s_idle)
  }

  // request data from memory
  io.mem.req.valid := (state === s_mem_req)
  io.mem.req.bits.addr := commandParser.io.addr
  val regAddrMask = if (coreParams.useRVE) (1 << 4) - 1 else (1 << 5) - 1
  io.mem.req.bits.tag := commandParser.io.memTag
  io.mem.req.bits.cmd := commandParser.io.memCmd // perform a load (M_XWR for stores)
  io.mem.req.bits.size := commandParser.io.size // 3 ld funct code
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := commandParser.io.memDprv
  io.mem.req.bits.dv := commandParser.io.memDv
  when(io.mem.req.fire) {
    state := s_mem_wait
  }

  // receive data from memory
  when(io.mem.resp.valid && state === s_mem_wait) {
    data := io.mem.resp.bits.data
    state := Mux(commandParser.io.load, s_resp, s_idle)
  }

  // respond
  io.resp.valid := (state === s_resp)
  io.resp.bits.data := data
  io.resp.bits.rd := commandParser.io.rd
  when(io.resp.fire) {
    state := s_idle
  }

  val ptw_req = io.ptw(0).req
  val tl_out_a = tl_out.a
  val tl_out_d = tl_out.d
  dontTouch(state)
  dontTouch(data)
  dontTouch(ptw_req)
  dontTouch(tl_out_a)
  dontTouch(tl_out_d)
}

class CommandParserModule(implicit val p: Parameters)
    extends Module
    with HasCoreParameters
    with MemoryOpConstants
    with ScalarOpConstants {
  // TODO add raw load/store support
  // val raw = (inst.opcode === "b0110011".U)
  val io = IO(new Bundle {
    val size = Output(UInt())
    val unsigned = Output(Bool())
    val load = Output(Bool())
    val rd = Output(UInt(5.W))
    val wdata = Output(UInt(xLen.W))
    val cmd = Input(new RoCCCommand)
    val addr = Output(UInt((2 * xLen).W))
    val local = Output(Bool())
    val state = Input(UInt(3.W))
    val memDprv = Output(Bool())
    val memDv = Output(Bool())
    val memTag = Output(UInt(5.W))
    val memCmd = Output(UInt(M_SZ.W))
  })
  dontTouch(io)
  val stateIsIdle = (io.state === 0.U)
  val reg_cmd = RegEnable(io.cmd, stateIsIdle)
  val cmd = Mux(stateIsIdle, io.cmd, reg_cmd)

  io.size := VecInit(cmd.inst.xs2, cmd.inst.xs1).asUInt
  io.unsigned := cmd.inst.xd
  io.load := (cmd.inst.opcode =/= "b1111011".U)

  // extended addressing
  val inst = cmd.inst.asUInt
  val imm = Wire(SInt((2 * xLen).W))
  imm := Mux(
    io.load,
    ImmGen(IMM_I, inst),
    ImmGen(IMM_S, inst)
  )
  val addr = Wire(UInt((2 * xLen).W))
  addr := Cat(cmd.rs3, cmd.rs1)
  io.addr := addr + imm.asUInt
  io.local := (cmd.rs3 === 0.U)

  // data
  io.wdata := cmd.rs2
  io.rd := cmd.inst.rd

  // memory
  io.memDprv := cmd.status.dprv
  io.memDv := cmd.status.dv
  val regAddrMask = if (coreParams.useRVE) (1 << 4) - 1 else (1 << 5) - 1
  io.memTag := cmd.inst.rd & regAddrMask.U
  io.memCmd := Mux(io.load, M_XRD, M_XWR) // TODO set for other load/store sizes
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
