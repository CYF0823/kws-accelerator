package acceleration
import chisel3._
import chisel3.util._

class BN_Unit(data_width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(data_width.W))
    val control = Input(UInt(2.W))
    val output  = Output(UInt(data_width.W))
    val wrAddr  = Input(UInt(2.W))
    val wrEna   = Input(Bool())
    val wrData  = Input(UInt(data_width.W))
  })

  val BN_bias = Reg(Vec(3,UInt(data_width.W)))

  val output_reg = RegInit(0.U(data_width.W))
  io.output := output_reg

  //out = a * b + c
  val FP16MAC = Module(new FP16MulAdder)
  FP16MAC.io.op := 0.U
  FP16MAC.io.roundingMode := 0.U
  FP16MAC.io.detectTininess := 0.U

  //write
  when(io.wrEna){
    BN_bias(io.wrAddr) := io.wrData
  }

  when(io.control === 0.U){
    FP16MAC.io.a := 0.U
    FP16MAC.io.b := 0.U
    FP16MAC.io.c := 0.U
    output_reg := io.input
  }
  .elsewhen(io.control === 1.U){
    FP16MAC.io.a := io.input
    FP16MAC.io.b := "h3c00".U //1.U
    FP16MAC.io.c := BN_bias(0)
    output_reg := FP16MAC.io.out
  }
  .elsewhen(io.control === 2.U){
    FP16MAC.io.a := io.input
    FP16MAC.io.b := "h3c00".U //1.U
    FP16MAC.io.c := BN_bias(1)
    output_reg := FP16MAC.io.out
  }
  .otherwise{
    FP16MAC.io.a := io.input
    FP16MAC.io.b := "h3c00".U //1.U
    FP16MAC.io.c := BN_bias(2)
    output_reg := FP16MAC.io.out
  }
}

class BN_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_PE   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(2.W)))
    val to_Relu6  = Output(Vec(12,UInt(width.W)))
    val wrAddr    = Input(Vec(12,UInt(2.W)))
    val wrEna     = Input(Vec(12,Bool()))
    val wrData    = Input(Vec(12,UInt(width.W)))
  })

  //val BN_Array = Vec(12,Module(new BN_Unit(data_width = width)))
  val BN_Array = Seq.fill(12)(Module(new BN_Unit(data_width = width)))

  //connection
  for(i <- 0 until 12){
    BN_Array(i).io.input := io.from_PE(i)
    BN_Array(i).io.control := io.control(i)
    BN_Array(i).io.wrAddr := io.wrAddr(i)
    BN_Array(i).io.wrEna := io.wrEna(i)
    BN_Array(i).io.wrData := io.wrData(i)
    io.to_Relu6(i) := BN_Array(i).io.output
  }

}

class Relu6_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(width.W))
    val control   = Input(UInt(1.W))
    val output = Output(UInt(width.W))
  })

  when(io.control === 0.U){
    io.output := io.input
  }
    //control === 1.U
  .otherwise{
    when(io.input(width - 1) === 1.U){
      io.output := 0.U
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input <= "h4600".U)){  //0 <= input <= 6
      io.output := io.input
    }
    .otherwise{
      io.output := "h4600".U
    }
  }
}

class Relu6_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(1.W)))
    val output = Output(Vec(12,UInt(width.W)))
  })

  //val Relu6_Array = Vec(12,Module(new Relu6_Unit(width = width)))
  val Relu6_Array = Seq.fill(12)(Module(new Relu6_Unit(width)))

  //connection
  for(i <- 0 until 12){
    Relu6_Array(i).io.input := io.input(i)
    Relu6_Array(i).io.control := io.control(i)
    io.output(i) := Relu6_Array(i).io.output
  }

}

class activation_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(width.W))
    val control   = Input(UInt(2.W))
    val output = Output(UInt(width.W))
  })

  val relu6 = Module(new Relu6_Unit(width = width))
  relu6.io.control := 1.U
  relu6.io.input := io.input

  //out = a * b + c
  val FP16MAC = Module(new FP16MulAdder)
  FP16MAC.io.op := 0.U
  FP16MAC.io.roundingMode := 0.U
  FP16MAC.io.detectTininess := 0.U


  val output_reg = RegInit(0.U(width.W))
  io.output := output_reg

  when(io.control === 0.U){
    FP16MAC.io.a := 0.U
    FP16MAC.io.b := 0.U
    FP16MAC.io.c := 0.U
    output_reg := io.input
  }
  //sigmoid
  .elsewhen(io.control === 1.U){
    when((io.input(width - 1) === 1.U) && (io.input >= "hc800".U)){ //input <= -8
      FP16MAC.io.a := 0.U
      FP16MAC.io.b := 0.U
      FP16MAC.io.c := 0.U
      output_reg := 0.U
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc800".U) && (io.input >= "hc700".U)){//-8 < input <= -7
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1e24".U //0.006
      FP16MAC.io.c := "h1cea".U //0.0048
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc700".U) && (io.input >= "hc600".U)){//-7 < input <= -6
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1624".U //0.0015
      FP16MAC.io.c := "h21f0".U //0.0116
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc600".U) && (io.input >= "hc500".U)){//-6 < input <= -5
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1c4d".U //0.0042
      FP16MAC.io.c := "h26f0".U //0.0271
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc500".U) && (io.input >= "hc400".U)){//-5 < input <= -4
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h21ae".U //0.0111
      FP16MAC.io.c := "h2bdf".U //0.0615
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc400".U) && (io.input >= "hc200".U)){//-4 < input <= -3
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2773".U //0.0291
      FP16MAC.io.c := "h303a".U //0.1322
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc200".U) && (io.input >= "hc000".U)){//-3 < input <= -2
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2c8c".U //0.0711
      FP16MAC.io.c := "h3419".U //0.2562
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc000".U) && (io.input >= "hbc00".U)){//-2 < input <= -1
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h30c8".U //0.1495
      FP16MAC.io.c := "h3692".U //0.4107
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hbc00".U)){//-1 < input < 0
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h3371".U //0.2326
      FP16MAC.io.c := "h37f0".U //0.4962
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input >= 0.U) && (io.input <= "h3c00".U)){//0 <= input <= 1
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h3371".U //0.2326
      FP16MAC.io.c := "h3807".U //0.5038
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h3c00".U) && (io.input <= "h4000".U)){//1 < input <= 2
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h30c8".U //0.1495
      FP16MAC.io.c := "h38b6".U //0.5893
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4000".U) && (io.input <= "h4200".U)){//2 < input <= 3
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2c8c".U //0.0711
      FP16MAC.io.c := "h39f3".U //0.7438
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4200".U) && (io.input <= "h4400".U)){//3 < input <= 4
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2773".U //0.0291
      FP16MAC.io.c := "h3af1".U //0.8678
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4400".U) && (io.input <= "h4500".U)){//4 < input <= 5
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h21ae".U //0.0111
      FP16MAC.io.c := "h3b82".U //0.9385
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4500".U) && (io.input <= "h4600".U)){//5 < input <= 6
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1c4d".U //0.0042
      FP16MAC.io.c := "h3bc8".U //0.9729
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4600".U) && (io.input <= "h4700".U)){//6 < input <= 7
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1624".U //0.0015
      FP16MAC.io.c := "h3be8".U //0.9884
      output_reg := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4700".U) && (io.input <= "h4800".U)) { //7 < input <= 8
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h10ea".U //0.0006
      FP16MAC.io.c := "h3bf6".U //0.9952
      output_reg := FP16MAC.io.out
    }
      .otherwise{
        FP16MAC.io.a := 0.U
        FP16MAC.io.b := 0.U
        FP16MAC.io.c := 0.U
        output_reg := "h3c00".U //1
      }
  }
  //tanh
  .elsewhen(io.control === 2.U){
    output_reg := io.input
    FP16MAC.io.a := 0.U
    FP16MAC.io.b := 0.U
    FP16MAC.io.c := 0.U
  }
  //relu6
  .otherwise{ //io.control === 3.U
    output_reg := relu6.io.output
    FP16MAC.io.a := 0.U
    FP16MAC.io.b := 0.U
    FP16MAC.io.c := 0.U
  }
}

class tanh_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(width.W))
    val output = Output(UInt(width.W))
  })

  //out = a * b + c
  val FP16MAC = Module(new FP16MulAdder)
  FP16MAC.io.op := 0.U
  FP16MAC.io.roundingMode := 0.U
  FP16MAC.io.detectTininess := 0.U

  when((io.input(width - 1) === 1.U) && (io.input >= "hc600".U)){ //input <= -6
    FP16MAC.io.a := 0.U
    FP16MAC.io.b := 0.U
    FP16MAC.io.c := 0.U
    io.output := "hbc00".U  //-1
  }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc600".U) && (io.input >= "hc500".U)){//-6 < input <= -5
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1c4d".U //0.0001
      FP16MAC.io.c := "h26f0".U //0.9996
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc500".U) && (io.input >= "hc400".U)){//-5 < input <= -4
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h21ae".U //0.0005
      FP16MAC.io.c := "h2bdf".U //0.9973
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc400".U) && (io.input >= "hc200".U)){//-4 < input <= -3
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2773".U //0.0040
      FP16MAC.io.c := "h303a".U //0.9838
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc200".U) && (io.input >= "hc000".U)){//-3 < input <= -2
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2c8c".U //0.0293
      FP16MAC.io.c := "h3419".U //0.9111
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hc000".U) && (io.input >= "hbc00".U)){//-2 < input <= -1
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h30c8".U //0.1940
      FP16MAC.io.c := "h3692".U //0.5999
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 1.U) && (io.input < "hbc00".U)){//-1 < input < 0
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h3371".U //0.7714
      FP16MAC.io.c := "h37f0".U //0.0475
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input >= 0.U) && (io.input <= "h3c00".U)){//0 <= input <= 1
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h3371".U //0.7714
      FP16MAC.io.c := "h3807".U //0.0475
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h3c00".U) && (io.input <= "h4000".U)){//1 < input <= 2
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h30c8".U //0.1940
      FP16MAC.io.c := "h38b6".U //0.5999
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4000".U) && (io.input <= "h4200".U)){//2 < input <= 3
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2c8c".U //0.0293
      FP16MAC.io.c := "h39f3".U //0.9111
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4200".U) && (io.input <= "h4400".U)){//3 < input <= 4
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h2773".U //0.0040
      FP16MAC.io.c := "h3af1".U //0.9838
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4400".U) && (io.input <= "h4500".U)){//4 < input <= 5
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h21ae".U //0.0005
      FP16MAC.io.c := "h3b82".U //0.9973
      io.output := FP16MAC.io.out
    }
    .elsewhen((io.input(width - 1) === 0.U) && (io.input > "h4500".U) && (io.input <= "h4600".U)){//5 < input <= 6
      FP16MAC.io.a := io.input
      FP16MAC.io.b := "h1c4d".U //0.0001
      FP16MAC.io.c := "h3bc8".U //0.9996
      io.output := FP16MAC.io.out
    }
    .otherwise{
      io.output := "h3c00".U //1
      FP16MAC.io.a := 0.U
      FP16MAC.io.b := 0.U
      FP16MAC.io.c := 0.U
    }
}

class accumulator_registers(data_width: Int,addr_width:Int) extends Module {
  val io = IO(new Bundle {
    val rdAddr  = Input(UInt(addr_width.W))
    val rdData  = Output(UInt(data_width.W))
    val wrEna   = Input(Bool())
    val wrData  = Input(UInt(data_width.W))
    val wrAddr  = Input(UInt(addr_width.W))
  })

  val register = RegInit(VecInit(Seq.fill(2^addr_width)(0.U(data_width.W))))

  //out = a * b + c
  val FP16MAC = Module(new FP16MulAdder)
  FP16MAC.io.op := 0.U
  FP16MAC.io.roundingMode := 0.U
  FP16MAC.io.detectTininess := 0.U

  io.rdData := register(io.rdAddr)
  when(io.wrEna){
    FP16MAC.io.a := io.wrData
    FP16MAC.io.b := "h3c00".U //1.U
    FP16MAC.io.c := register(io.wrAddr)
    register(io.wrAddr) := FP16MAC.io.out
  }
    .otherwise{
      FP16MAC.io.a := 0.U
      FP16MAC.io.b := 0.U
      FP16MAC.io.c := 0.U
    }
}

class ht(data_width: Int,addr_width:Int) extends Module {
  val io = IO(new Bundle {
    val to_PE = Output(Vec(12,UInt(data_width.W)))
    val to_PE_control = Input(UInt(3.W))
    val rdData = Output(UInt(data_width.W))
    val rdAddr = Input(UInt(addr_width.W))
    val wrEna = Input(Bool())
    val wrData = Input(UInt(data_width.W))
    val wrAddr = Input(UInt(addr_width.W))
  })

  val register = RegInit(VecInit(Seq.fill(64)(0.U(data_width.W))))

  //out = a * b + c
  val FP16MAC = Module(new FP16MulAdder)
  FP16MAC.io.op := 0.U
  FP16MAC.io.roundingMode := 0.U
  FP16MAC.io.detectTininess := 0.U

  //to_PE
  when(io.to_PE_control === 0.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(i.U)
    }
  }
  .elsewhen(io.to_PE_control === 1.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(12.U + i.U)
    }
  }
  .elsewhen(io.to_PE_control === 2.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(24.U + i.U)
    }
  }
  .elsewhen(io.to_PE_control === 3.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(36.U + i.U)
    }
  }
  .elsewhen(io.to_PE_control === 4.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(48.U + i.U)
    }
  }
  .elsewhen(io.to_PE_control === 5.U){
    for (i <- 0 until 4){
      io.to_PE(i) := register(60.U + i.U)
    }
    for (i <- 4 until 12){
      io.to_PE(i) := 0.U
    }
  }
    .otherwise{
      for (i <- 0 until 12){
        io.to_PE(i) := 0.U
      }
    }

  //read
  io.rdData := register(io.rdAddr)
  //write
  when(io.wrEna) {
    FP16MAC.io.a := io.wrData
    FP16MAC.io.b := "h3c00".U //1.U
    FP16MAC.io.c := register(io.wrAddr)
    register(io.wrAddr) := FP16MAC.io.out
  }
    .otherwise{
      FP16MAC.io.a := 0.U
      FP16MAC.io.b := 0.U
      FP16MAC.io.c := 0.U
    }
}

class EW_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val ht_1_input    = Input(UInt(width.W))
    val Zt_input      = Input(UInt(width.W))
    val Rt_input      = Input(UInt(width.W))
    val Whxt_input    = Input(UInt(width.W))
    val Uhht_1_input  = Input(UInt(width.W))
    val output        = Output(UInt(width.W))
  })

  val reg_before_tanh = RegInit(0.U(width.W))
  val reg_Zt        = RegInit(0.U(width.W))
  val reg_ht_1      = RegInit(0.U(width.W))
  val reg_tanh      = RegInit(0.U(width.W))
  val reg_1_Zt      = RegInit(0.U(width.W))
  val reg_Zt_ht_1   = RegInit(0.U(width.W))
  val reg_output    = RegInit(0.U(width.W))

  //out = a * b + c
  val FP16MAC1 = Module(new FP16MulAdder)
  FP16MAC1.io.op := 0.U
  FP16MAC1.io.roundingMode := 0.U
  FP16MAC1.io.detectTininess := 0.U
  val FP16MAC2 = Module(new FP16MulAdder)
  FP16MAC2.io.op := 0.U
  FP16MAC2.io.roundingMode := 0.U
  FP16MAC2.io.detectTininess := 0.U
  val FP16MAC3 = Module(new FP16MulAdder)
  FP16MAC3.io.op := 0.U
  FP16MAC3.io.roundingMode := 0.U
  FP16MAC3.io.detectTininess := 0.U
  val FP16MAC4 = Module(new FP16MulAdder)
  FP16MAC4.io.op := 0.U
  FP16MAC4.io.roundingMode := 0.U
  FP16MAC4.io.detectTininess := 0.U

  //reg_before_tanh := io.Rt_input * io.Uhht_1_input + io.Whxt_input
  FP16MAC1.io.a := io.Rt_input
  FP16MAC1.io.b := io.Uhht_1_input
  FP16MAC1.io.c := io.Whxt_input
  reg_before_tanh := FP16MAC1.io.out

  reg_Zt        := io.Zt_input
  reg_ht_1      := io.ht_1_input
  val tanh = Module(new tanh_Unit(width = width))
  tanh.io.input := reg_before_tanh
  reg_tanh      := tanh.io.output

  //reg_1_Zt    := 1.U - reg_Zt
  val reg_Zt_inv = Cat(~reg_Zt(width - 1),reg_Zt(width - 2, 0)) //inverse reg_Zt
  FP16MAC2.io.a := reg_Zt_inv
  FP16MAC2.io.b := "h3c00".U //1.U
  FP16MAC2.io.c := "h3c00".U //1.U
  reg_1_Zt      := FP16MAC2.io.out

  //reg_Zt_ht_1   := reg_Zt * reg_ht_1
  FP16MAC3.io.a := reg_Zt
  FP16MAC3.io.b := reg_ht_1
  FP16MAC3.io.c := 0.U
  reg_Zt_ht_1   := FP16MAC3.io.out

  //reg_output  := reg_tanh * reg_1_Zt + reg_Zt_ht_1
  FP16MAC4.io.a := reg_tanh
  FP16MAC4.io.b := reg_1_Zt
  FP16MAC4.io.c := reg_Zt_ht_1
  reg_output   := FP16MAC4.io.out

  io.output   := reg_output
}

