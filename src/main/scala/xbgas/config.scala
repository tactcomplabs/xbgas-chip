package xbgas

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import chisel3._

object XbgasOpcodeSet {
  def integerLoad = new OpcodeSet(Seq("b1110111".U))
}

class WithXbgasRocc extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val xbgas = LazyModule.apply(new XbgasAccel(XbgasOpcodeSet.integerLoad)(p))
      xbgas
    }
  )
})