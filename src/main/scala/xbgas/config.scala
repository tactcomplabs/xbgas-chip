package xbgas

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import chisel3._

object XbgasOpcodeSet {
  def integerLoad = new OpcodeSet(Seq("b1110111".U))
  def integerStore = new OpcodeSet(Seq("b1111011".U))
  def rawInteger = new OpcodeSet(Seq("b0110011".U))
  def all = integerLoad | integerStore | rawInteger
}

class WithXbgasRocc extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val xbgas = LazyModule.apply(new XbgasAccel(XbgasOpcodeSet.all)(p))
      xbgas
    }
  )
})