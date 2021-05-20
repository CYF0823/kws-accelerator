package acceleration

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

object acceleratorGen extends App{

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(ChiselGeneratorAnnotation(() => new Top(8))))
}
