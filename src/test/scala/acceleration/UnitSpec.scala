package acceleration

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class BNSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BN Unit"
  it should "add bias" in{
    test(new BN_Unit(8)) { c =>
      for(i <- 0 until 3){
        c.io.wrEna.poke(true.B)
        c.io.wrAddr.poke(i.U)
        c.io.wrData.poke((i+1).U)
        c.clock.step(1)
      }

      c.io.wrEna.poke(false.B)
      for(i <- 0 until 3){
        c.io.control.poke((i+1).U)
        c.io.input.poke(5.U)
        c.io.output.expect((6+i).U)
        c.clock.step(1)
      }
    }
  }
}

class RElu6Spec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "RElu6 Unit"
  it should "RElu6 function" in{
    test(new Relu6_Unit(8)) { c =>
      c.io.control.poke(0.U)
      c.io.input.poke(15.U)
      c.io.output.expect(15.U)
      c.clock.step(1)

      c.io.control.poke(1.U)
      c.io.input.poke(3.U)
      c.io.output.expect(3.U)
      c.clock.step(1)

      c.io.control.poke(1.U)
      c.io.input.poke(10.U)
      c.io.output.expect(6.U)
      c.clock.step(1)
    }
  }
}

class accumulatorSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "accumulator Unit"
  it should "accumulating" in{
    test(new ht(8,6)) { c =>
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
