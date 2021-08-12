package acceleration

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class HardfloatSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "hard float"
  it should "float MAC" in{
    test(new FP32MulAdder) { c =>
      c.io.op.poke(0.U)
      c.io.roundingMode.poke(0.U)
      c.io.detectTininess.poke(0.U)
      //out = a * b + c
      c.io.a.poke("h3fc0_0000".U)   //1.5
      c.io.b.poke("h4020_0000".U)   //2.5
      c.io.c.poke("h4070_0000".U)   //3.75
      c.io.out.expect("h40f0_0000".U)    //7.5

      c.io.a.poke("h3dfb_e76c".U)   //0.123
      c.io.b.poke("h3ee9_78d4".U)   //0.456
      c.io.c.poke("h3fe4_fdf3".U)   //1.789
      c.io.out.expect("h3fec_2bd7".U)    //1.845038

      c.io.a.poke("hc019_9999".U)   //-2.4
      c.io.b.poke("h3fbd_70a3".U)   //1.48
      c.io.c.poke("hc086_b851".U)   //-4.21
      c.io.out.expect("hc0f8_624c".U)    //-7.762,a little error

    }
  }
}