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
      lgSize = commandParser.io.size,
      data = commandParser.io.wdata
    )
    ._2
  val putPartial = edgesOut
    .Put(
      fromSource = 0.U,
      toAddress = commandParser.io.addr,
      lgSize = commandParser.io.size,
      data = commandParser.io.wdata,
      mask = commandParser.io.mask
    )
    ._2
  tl_out.a.bits := Mux(
    commandParser.io.load,
    get,
    Mux(commandParser.io.partial, putPartial, put)
  )
  when(tl_out.a.fire) {
    state := s_tl_wait
  }

  // receive data from MMIO
  tl_out.d.ready := (state === s_tl_wait)
  val data = Reg(UInt(xLen.W))
  val addr = commandParser.io.addr(coreMaxAddrBits - 1, 0)
  val loadGenData = new LoadGen(
    commandParser.io.size,
    commandParser.io.signed,
    addr,
    tl_out.d.bits.data,
    false.B,
    coreDataBytes
  ).data
  val coreDataBytesWire = Wire(UInt())
  coreDataBytesWire := coreDataBytes.U
  dontTouch(coreDataBytesWire)
  data := loadGenData
  val loadGenDataWire = Wire(UInt())
  loadGenDataWire := loadGenData
  dontTouch(loadGenDataWire)
  dontTouch(addr)
  when(tl_out.d.fire) {
    state := Mux(
      tl_out.d.bits.opcode === TLMessages.AccessAckData,
      s_resp,
      s_idle
    )
  }

  // respond
  io.resp.valid := (state === s_resp)
  io.resp.bits.data := data
  io.resp.bits.rd := commandParser.io.rd
  io.resp.bits.extd := commandParser.io.extd
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
    val size = Output(UInt(2.W))
    val load = Output(Bool())
    val signed = Output(Bool())
    val rd = Output(UInt(5.W))
    val wdata = Output(UInt(xLen.W))
    val mask = Output(UInt(coreDataBytes.W))
    val partial = Output(Bool())
    val extd = Output(Bool())
    val cmd = Input(new RoCCCommand)
    val addr = Output(UInt((2 * xLen).W))
    val state = Input(UInt(3.W))
  })
  dontTouch(io)
  assert(io.cmd.rs3 =/= 0.U)
  val stateIsIdle = (io.state === 0.U)
  val reg_cmd = RegEnable(io.cmd, stateIsIdle)
  val cmd = Mux(stateIsIdle, io.cmd, reg_cmd)

  val (extendedDestination, load, size) = decodeXbgasInstruction(
    cmd.inst.asUInt
  )
  io.size := size
  io.load := load
  io.extd := extendedDestination

  // extended addressing
  io.addr := Cat(cmd.rs3, cmd.rs1)

  // data
  val storeGen = new StoreGen(
    size,
    cmd.rs1(coreMaxAddrBits - 1, 0),
    cmd.rs2,
    coreDataBytes
  )

  io.wdata := storeGen.data
  io.mask := storeGen.mask
  io.partial := size =/= 3.U
  io.signed := !cmd.inst.asUInt(14)
  io.rd := cmd.inst.rd

  def decodeXbgasInstruction(inst: UInt) = {
    val opcode = inst(6, 0)
    val funct3 = inst(14, 12)
    val funct7 = inst(31, 25)

    val load = Mux1H(
      Seq(
        XbgasOpcodeSet.integerLoad.matches(opcode) -> true.B,
        XbgasOpcodeSet.integerStore.matches(opcode) -> false.B,
        XbgasOpcodeSet.rawInteger.matches(opcode) -> (funct7 === "b1010101".U)
      )
    )

    val extd = funct3 === 7.U
    val size = funct3(1, 0)
    (extd, load, size)
  }
}
