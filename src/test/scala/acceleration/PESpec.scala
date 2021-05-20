
package acceleration

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class PESpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PE width=8, row = 1,column = 1"
  it should "dw layer" in{
    test(new PE(32,1,1)) { c =>
      c.io.control_signal.control.poke(3.U)
      c.io.control_signal.mask.poke("b1111_1111_1111".U)
      c.io.control_signal.L0index.poke(1.U)
      c.io.control_signal.count.poke(2.U)
      c.io.FromL1.poke("h4040_0000".U)  //3.U
      c.clock.step(4)
      c.io.control_signal.control.poke(1.U)
      c.io.control_signal.mask.poke("b1111_1111_1111".U)
      c.io.FromAbovePE.poke("h3F80_0000".U) //1.U
      c.io.FromL1.poke("h4000_0000".U)  //2.U
      c.clock.step(3)
      c.io.ToBelowPE.expect("h40e0_0000".U)
    }
  }

  it should "pw layer" in{
    test(new PE(32,2,1)) { c =>
      c.io.control_signal.control.poke(3.U)
      c.io.control_signal.mask.poke("b1111_1111_1111".U)
      c.io.control_signal.L0index.poke(4.U)
      c.io.control_signal.count.poke(2.U)
      c.io.FromL1.poke("h3f4c_cccc".U) //0.8
      c.clock.step(4)
      c.io.control_signal.control.poke(2.U)
      c.io.control_signal.mask.poke("b1111_1111_1111".U)
      c.io.FromLeftPE.poke("hbe99_9999".U) //-0.3
      c.clock.step(3)
      c.io.ToBelowPE.expect("hbe75_c28d".U) //-0.24
    }
  }
}
