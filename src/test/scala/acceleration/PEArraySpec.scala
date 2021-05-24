
package acceleration

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class PEArraySpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PE width=8, row = 1,column = 1"
  it should "dw layer" in{
    test(new PEArray(32)) { c =>
      for(i <- 0 until 3){
        c.io.PE_control(i).control.poke(3.U)
        c.io.PE_control(i).mask.poke("b1111_1111_1111".U)
        c.io.PE_control(i).L0index.poke(i.U)
        c.io.PE_control(i).count.poke(2.U)
      }
      for(i <- 0 until 12){
        c.io.From_above(i).poke("h4040_0000".U)  //3.U
      }
      c.clock.step(4)

      for(i <- 0 until 3){
        c.io.PE_control(i).control.poke(1.U)
        c.io.PE_control(i).mask.poke("b1111_1111_1111".U)
      }
      for(i <- 0 until 12){
        c.io.From_above(i).poke("h40a0_0000".U) //5.U
      }

      c.clock.step(3)
      for(i <- 0 until 12){
        c.io.To_below(i).expect("h4170_0000".U) //15.U
      }
      c.clock.step(1)
      for(i <- 0 until 12){
        c.io.To_below(i).expect("h41F0_0000".U) //30.U
      }
      c.clock.step(5)
      for(i <- 0 until 12){
        c.io.To_below(i).expect("h4234_0000".U) //45.U
      }
    }
  }
}
