package acceleration

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class BNSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BN Unit"
  it should "add bias" in{
    test(new BN_Unit(16)) { c =>

      c.io.wrEna.poke(true.B)
      c.io.wrAddr.poke(1.U)
      c.io.wrData.poke("h4000".U) //2
      c.clock.step(1)

      c.io.wrEna.poke(false.B)
      c.io.control.poke(2.U)
      c.io.input.poke("h4400".U) //4
      c.io.output.expect("h4600".U) //6
      c.clock.step(1)
    }
  }
}

class RElu6Spec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "RElu6 Unit"
  it should "RElu6 function" in{
    test(new Relu6_Unit(16)) { c =>
      c.io.control.poke(0.U)
      c.io.input.poke(15.U)
      c.io.output.expect(15.U)
      c.clock.step(1)

      c.io.control.poke(1.U)
      c.io.input.poke("hc200".U)
      c.io.output.expect(0.U)
      c.clock.step(1)

      c.io.control.poke(1.U)
      c.io.input.poke("h1e24".U)
      c.io.output.expect("h1e24".U)
      c.clock.step(1)

      c.io.control.poke(1.U)
      c.io.input.poke("h4800".U)
      c.io.output.expect("h4600".U)
    }
  }
}

class accumulatorSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "accumulator Unit"
  it should "accumulating" in{
    test(new ht(16,6)) { c =>
      for(i <- 1 until 11){
        for(j <- 0 until 64){
          c.io.wrEna.poke(true.B)
          c.io.wrData.poke(i.U)
          c.io.wrAddr.poke(j.U)
          c.clock.step(1)
        }
      }

      c.io.wrEna.poke(false.B)
      for(j <- 0 until 64){
        c.io.rdAddr.poke(j.U)
        c.io.rdData.expect((55.U))
        c.clock.step(1)
      }

      for(j <- 0 until 64){
        c.io.wrEna.poke(true.B)
        c.io.wrData.poke(j.U)
        c.io.wrAddr.poke(j.U)
        c.clock.step(1)
      }

      c.io.wrEna.poke(false.B)

      c.io.to_PE_control.poke(0.U)
      c.io.to_PE(0).expect(55.U)
      c.io.to_PE(1).expect(56.U)
      c.io.to_PE(2).expect(57.U)

      c.clock.step(1)

      c.io.to_PE_control.poke(5.U)
      c.io.to_PE(0).expect(115.U)
      c.io.to_PE(1).expect(116.U)
      c.io.to_PE(2).expect(117.U)
      c.io.to_PE(3).expect(118.U)
      c.io.to_PE(4).expect(0.U)
      c.io.to_PE(5).expect(0.U)
      c.io.to_PE(6).expect(0.U)
      c.io.to_PE(7).expect(0.U)
      c.io.to_PE(8).expect(0.U)
      c.io.to_PE(9).expect(0.U)
      c.io.to_PE(10).expect(0.U)
      c.io.to_PE(11).expect(0.U)
    }
  }
}
