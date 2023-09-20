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
  val extd = Bool()
  val ers = Bits(ERS_X.getWidth.W)

  def default: List[BitPat] =
                //           jal                                                                         renf1               fence.i
                //   val     | jalr                                                                      | renf2             |
                //   | fp_val| | renx2                                                                   | | renf3           |
                //   | | rocc| | | renx1               s_alu1                              mem_val       | | | wfd           |
                //   | | | br| | | | scie      s_alu2  |       imm    dw     alu           | mem_cmd     | | | | mul         |       exd
                //   | | | | | | | | | zbk     |       |       |      |      |             | |           | | | | | div       | fence | ers
                //   | | | | | | | | | | zkn   |       |       |      |      |             | |           | | | | | | wxd     | | amo | |
                //   | | | | | | | | | | | zks |       |       |      |      |             | |           | | | | | | |       | | | dp| |
                List(N,X,X,X,X,X,X,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,   N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X,X,ERS_X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, zbk, zkn, zks, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp, extd, ers)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),

    LB->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    LH->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    LW->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),

    LUI->       List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N,N,ERS_NONE),

    ECALL->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    EBREAK->    List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    MRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    WFI->       List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,N,ERS_NONE),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,N,ERS_NONE),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,N,ERS_NONE),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,N,ERS_NONE),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,N,ERS_NONE),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,N,ERS_NONE))
}

class CeaseDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CEASE->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}


class FenceIDecode(flushDCache: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N,N,ERS_NONE))
}

class ConditionalZeroDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CZERO_EQZ-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZEQZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CZERO_NEZ-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZNEZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class CFlushDecode(supportsFlushLine: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private def zapRs1(x: BitPat) = if (supportsFlushLine) x else BitPat(x.value.U)

  val table: Array[(BitPat, List[BitPat])] = Array(
    zapRs1(CFLUSH_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    zapRs1(CDISCARD_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class SVMDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class SDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class HypervisorDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(

    HFENCE_VVMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEV,  N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    HFENCE_GVMA->List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEG,  N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),

    HLV_B ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLV_BU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLV_H ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLV_HU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLVX_HU->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLV_W->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HLVX_WU->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),

    HSV_B->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    HSV_H->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    HSV_W->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class DebugDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class NMIDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MNRET->     List(Y,N,N,N,N,N,N,X,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE))
}

class I32Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SLLI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SRLI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SRAI->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class I64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class Hypervisor64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    HLV_D->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE),
    HSV_D->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N,N,ERS_NONE),
    HLV_WU->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N,N,ERS_NONE))
}

class MDecode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MULH->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,N,ERS_NONE),

    DIV->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    REM->       List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    REMU->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class M64Decode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,N,ERS_NONE),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    REMW->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ADecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE))
}

class A64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,N,ERS_NONE))
}

class HDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_H_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSGNJ_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSGNJX_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSGNJN_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMIN_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMAX_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FADD_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSUB_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMUL_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMADD_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMSUB_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FNMADD_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FNMSUB_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCLASS_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FMV_X_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_W_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_WU_H-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FEQ_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FLT_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FLE_H->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FMV_H_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_H_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_H_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FLH->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSH->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FDIV_H->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSQRT_H->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE))
}

class FDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FMV_X_W->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FMV_W_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FLW->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSW->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE))
}

class DDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FLD->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSD->       List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE))
}

class HDDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_D_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_H_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE))
}

class H64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_H->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_LU_H-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_H_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_H_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE))
}

class F64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE))
}

class D64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,N,ERS_NONE),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,N,N,N,N,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,N,ERS_NONE))
}

class SCIEDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SCIE.opcode->
                List(Y,N,N,N,N,N,Y,Y,Y,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

trait UsesABLUFN {
  val aluFn = ABLUFN()
}

class ZBADecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SH1ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH1ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SH2ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH2ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SH3ADD ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR, aluFn.FN_SH3ADD,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBA64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ADD_UW ->   List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_ADDUW   ,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SLLI_UW ->  List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_64,aluFn.FN_SLLIUW  ,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SH1ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH1ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SH2ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH2ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SH3ADD_UW-> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_64,aluFn.FN_SH3ADDUW,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// In both Zbb and Zbkb
class ZBBNDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ANDN ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ANDN,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ORN  ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ORN ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    XNOR ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XNOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// In both Zbb and Zbkb
class ZBBRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ROR ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ROL ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ROL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBR32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.RORI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBR64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    RORI ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    RORW ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    ROLW ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_ROL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    RORIW ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_32, aluFn.FN_ROR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// Only in Zbb
class ZBBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLZ ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLZ ,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CTZ ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CTZ ,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CPOP ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CPOP,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBC64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLZW ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CLZ ,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CTZW ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CTZ ,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CPOPW ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_32,aluFn.FN_CPOP,      N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// Only in Zbb
class ZBBMDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MAX ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MAX ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MAXU ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MAXU,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MIN ->      List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MIN ,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    MINU ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_MINU,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// Only in Zbb
class ZBBSEDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SEXT_H ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_SEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SEXT_B ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_SEXTB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBZE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ZEXT_H ->   List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ZEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBZE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.ZEXT_H ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ZEXTH,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBBORCBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    ORC_B ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ORCB,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// In both Zbb and Zbkb
class ZBBREV864Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    REV8 ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_REV8,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// In both Zbb and Zbkb
class ZBBREV832Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.REV8->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X,A1_RS1, IMM_X, DW_XPR,aluFn.FN_REV8,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// Only in Zbkb
class ZBKBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    PACK ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_PACK,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    PACKH ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_PACKH,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BREV8 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BREV8,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBKB64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    PACKW ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_32, aluFn.FN_PACK,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBKB32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.ZIP ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_X, aluFn.FN_ZIP,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.UNZIP ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X  ,A1_RS1, IMM_X, DW_X, aluFn.FN_UNZIP,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

// also in Zbkc but Zbkc does not have CLMULR
class ZBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLMUL ->    List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMUL,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    CLMULH ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMULH, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBCRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CLMULR ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_CLMULR, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBKXDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    XPERM8 ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XPERM8, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    XPERM4 ->   List(Y,N,N,N,N,N,Y,Y,N,Y,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_XPERM4, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BCLR ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BEXT ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BINV ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BSET ->     List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBS32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.BCLRI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.BEXTI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.BINVI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.BSETI ->
                List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZBS64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BCLRI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BCLR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BEXTI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BEXT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BINVI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BINV,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    BSETI ->    List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_BSET,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZKND32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.AES32DSI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DS, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.AES32DSMI->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DSM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}
class ZKND64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AES64DS ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DS, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AES64DSM -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_DSM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AES64IM ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_IM, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AES64KS1I ->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_IMM,A1_RS1, IMM_I, DW_XPR,aluFn.FN_AES_KS1,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AES64KS2 -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_KS2,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}
class ZKNE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.AES32ESI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ES, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.AES32ESMI ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ESM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}
class ZKNE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AES64ES ->  List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ES, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    AES64ESM -> List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_XPR,aluFn.FN_AES_ESM,N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZKNHDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SHA256SIG0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA256SIG1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA256SUM0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA256SUM1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA256_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}
class ZKNH32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SHA512SIG0L ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SHA512SIG1L ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SHA512SIG0H ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SHA512SIG1H ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SHA512SUM0R ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    Instructions32.SHA512SUM1R ->
                List(Y,N,N,N,N,N,Y,Y,N,N,Y,N,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}
class ZKNH64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SHA512SIG0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA512SIG1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SIG1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA512SUM0->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM0,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SHA512SUM1->List(Y,N,N,N,N,N,N,Y,N,N,Y,N,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SHA512_SUM1,N,M_X,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZKSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SM4ED ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,Y,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM4ED,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SM4KS ->    List(Y,N,N,N,N,N,Y,Y,N,N,N,Y,A2_RS2,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM4KS,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SM3P0 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,Y,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM3P0,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    SM3P1 ->    List(Y,N,N,N,N,N,N,Y,N,N,N,Y,A2_X  ,A1_RS1, IMM_X, DW_X,  aluFn.FN_SM3P1,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE))
}

class ZXBGASDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    EADDI ->      List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_BOTH),//ers1 needed only
    EADDIE ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,Y,ERS_NONE),
    EADDIX ->     List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,Y,ERS_BOTH),//ers1 needed only
    ELD->         List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_XRD,    N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_EQUAL),
    ESD->         List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   N,M_XWR,    N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_EQUAL),
  )
}

class RoCCDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    // CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE),
    // CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N,N,ERS_NONE)
    )
}
