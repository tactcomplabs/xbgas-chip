// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.scie.SCIE
import Instructions._
import CustomInstructions._

abstract trait DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends Bundle {

  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val scie = Bool()
  val zbk = Bool()
  val zkn = Bool()
  val zks = Bool()
  val sel_alu2 = Bits(A2_X.getWidth.W)
  val sel_alu1 = Bits(A1_X.getWidth.W)
  val sel_imm = Bits(IMM_X.getWidth.W)
  val alu_dw = Bool()
  val alu_fn = Bits(aluFn.FN_X.getWidth.W)
  val mem = Bool()
  val mem_cmd = Bits(M_SZ.W)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(CSR.SZ.W)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()
  val edp = Bits(EDP_X.getWidth.W)

  def default: List[BitPat] =
                //           jal                                                                         renf1               fence.i
                //   val     | jalr                                                                      | renf2             |
                //   | fp_val| | renx2                                                                   | | renf3           |
                //   | | rocc| | | renx1               s_alu1                              mem_val       | | | wfd           |
                //   | | | br| | | | scie      s_alu2  |       imm    dw     alu           | mem_cmd     | | | | mul         |       edp
                //   | | | | | | | | | zbk     |       |       |      |      |             | |           | | | | | div       | fence |
                //   | | | | | | | | | | zkn   |       |       |      |      |             | |           | | | | | | wxd     | | amo |
                //   | | | | | | | | | | | zks |       |       |      |      |             | |           | | | | | | |       | | | dp|
                List(N,X,X,X,X,X,X,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,   N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X,EDP_NORM)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, zbk, zkn, zks, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp, edp)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),

    LB->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    LH->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    LW->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),

    LUI->       List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N,EDP_NORM),

    ECALL->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    EBREAK->    List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    MRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    WFI->       List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,EDP_NORM),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,EDP_NORM),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,EDP_NORM),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,EDP_NORM),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,EDP_NORM),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,EDP_NORM))
}

class CeaseDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CEASE->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}


class FenceIDecode(flushDCache: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N,EDP_NORM))
}

class ConditionalZeroDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CZERO_EQZ-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZEQZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CZERO_NEZ-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZNEZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class CFlushDecode(supportsFlushLine: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private def zapRs1(x: BitPat) = if (supportsFlushLine) x else BitPat(x.value.U)

  val table: Array[(BitPat, List[BitPat])] = Array(
    zapRs1(CFLUSH_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    zapRs1(CDISCARD_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class SVMDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class SDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class HypervisorDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(

    HFENCE_VVMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEV,  N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    HFENCE_GVMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEG,  N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),

    HLV_B ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLV_BU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLV_H ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLV_HU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLVX_HU->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLV_W->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HLVX_WU->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),

    HSV_B->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    HSV_H->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    HSV_W->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class DebugDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class NMIDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MNRET->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM))
}

class I32Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SLLI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SRLI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SRAI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class I64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class Hypervisor64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    HLV_D->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM),
    HSV_D->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,EDP_NORM),
    HLV_WU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,EDP_NORM))
}

class MDecode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,EDP_NORM),
    MULH->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,EDP_NORM),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,EDP_NORM),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,EDP_NORM),

    DIV->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    REM->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    REMU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class M64Decode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,EDP_NORM),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    REMW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ADecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM))
}

class A64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,EDP_NORM))
}

class HDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_H_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSGNJ_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSGNJX_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSGNJN_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMIN_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMAX_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FADD_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSUB_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMUL_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMADD_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMSUB_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FNMADD_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FNMSUB_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCLASS_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FMV_X_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_W_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_WU_H-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FEQ_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FLT_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FLE_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FMV_H_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_H_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_H_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FLH->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSH->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FDIV_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSQRT_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM))
}

class FDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FMV_X_W->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FMV_W_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FLW->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSW->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM))
}

class DDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FLD->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSD->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM))
}

class HDDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_D_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_H_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM))
}

class H64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_LU_H-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_H_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_H_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM))
}

class F64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,EDP_NORM))
}

class D64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,EDP_NORM),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,EDP_NORM))
}

class SCIEDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SCIE.opcode->
                List(Y,N,N,N,N,N,Y,Y,Y,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

trait UsesABLUFN {
  val aluFn = ABLUFN()
}

class ZBADecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SH1ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH1ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SH2ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH2ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SH3ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH3ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBA64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ADD_UW ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_ADDUW   ,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SLLI_UW ->  List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_64,aluFn.FN_SLLIUW  ,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SH1ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH1ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SH2ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH2ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SH3ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH3ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// In both Zbb and Zbkb
class ZBBNDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ANDN ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ANDN,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ORN  ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ORN ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    XNOR ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XNOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// In both Zbb and Zbkb
class ZBBRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ROR ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ROL ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ROL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBR32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.RORI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBR64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    RORI ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    RORW ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    ROLW ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_ROL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    RORIW ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_32, aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// Only in Zbb
class ZBBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLZ ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLZ ,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CTZ ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CTZ ,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CPOP ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CPOP,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBC64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLZW ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CLZ ,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CTZW ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CTZ ,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CPOPW ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CPOP,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// Only in Zbb
class ZBBMDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MAX ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MAX ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    MAXU ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MAXU,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    MIN ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MIN ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    MINU ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MINU,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// Only in Zbb
class ZBBSEDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SEXT_H ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_SEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SEXT_B ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_SEXTB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBZE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ZEXT_H ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ZEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBZE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.ZEXT_H ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ZEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBBORCBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ORC_B ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ORCB,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// In both Zbb and Zbkb
class ZBBREV864Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    REV8 ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_REV8,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// In both Zbb and Zbkb
class ZBBREV832Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.REV8->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_REV8,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// Only in Zbkb
class ZBKBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    PACK ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_PACK,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    PACKH ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_PACKH,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BREV8 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BREV8,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBKB64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    PACKW ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_PACK,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBKB32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.ZIP ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_X, aluFn.FN_ZIP,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.UNZIP ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_X, aluFn.FN_UNZIP,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

// also in Zbkc but Zbkc does not have CLMULR
class ZBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLMUL ->    List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMUL,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    CLMULH ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMULH, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBCRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLMULR ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMULR, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBKXDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    XPERM8 ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XPERM8, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    XPERM4 ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XPERM4, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BCLR ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BEXT ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BINV ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BSET ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBS32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.BCLRI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.BEXTI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.BINVI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.BSETI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZBS64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BCLRI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BEXTI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BINVI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    BSETI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZKND32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.AES32DSI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DS, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.AES32DSMI->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DSM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}
class ZKND64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AES64DS ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DS, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AES64DSM -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DSM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AES64IM ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_IM, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AES64KS1I ->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_AES_KS1,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AES64KS2 -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_KS2,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}
class ZKNE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.AES32ESI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ES, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.AES32ESMI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ESM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}
class ZKNE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AES64ES ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ES, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    AES64ESM -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ESM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZKNHDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SHA256SIG0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA256SIG1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA256SUM0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA256SUM1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}
class ZKNH32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SHA512SIG0L ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SHA512SIG1L ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SHA512SIG0H ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SHA512SIG1H ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SHA512SUM0R ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    Instructions32.SHA512SUM1R ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}
class ZKNH64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SHA512SIG0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA512SIG1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA512SUM0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SHA512SUM1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZKSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SM4ED ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,Y,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM4ED,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SM4KS ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,Y,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM4KS,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SM3P0 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,Y,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM3P0,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    SM3P1 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,Y,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM3P1,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM))
}

class ZXBGASDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    EADDI ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_ADDI),//ers1 needed only
    EADDIE ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_ADDIE),
    EADDIX ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_ADDIX),//ers1 needed only
    ELD->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELW->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELH->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELHU->        List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELB->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELBU->        List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEM),
    ELE->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_MEME),
    ESD->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_MEM),
    ESW->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_MEM),
    ESH->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_MEM),
    ESB->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_MEM),
    ESE->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_MEME),
    )
}

class RoCCDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    // CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM),
    // CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,EDP_NORM)
    )
}
