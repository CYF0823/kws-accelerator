package acceleration
import chisel3._

class BN_Unit(data_width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(data_width.W))
    val control = Input(UInt(2.W))
    val output  = Output(UInt(data_width.W))
    val wrAddr  = Input(UInt(2.W))
    val wrEna   = Input(Bool())
    val wrData  = Input(UInt(data_width.W))
  })

  val BN_bias = Reg(Vec(3,UInt(8.W)))

  //write
  when(io.wrEna){
    BN_bias(io.wrAddr) := io.wrData
  }

  when(io.control === 0.U){
    io.output := io.input
  }
  .elsewhen(io.control === 1.U){
    io.output := io.input + BN_bias(0)
  }
  .elsewhen(io.control === 2.U){
    io.output := io.input + BN_bias(1)
  }
  .otherwise{
    io.output := io.input + BN_bias(2)
  }
}

class BN_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_PE   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(2.W)))
    val to_Relu6 = Output(Vec(12,UInt(width.W)))
  })

  //val BN_Array = Vec(12,Module(new BN_Unit(data_width = width)))
  val BN_Array = Seq.fill(12)(Module(new BN_Unit(data_width = width)))

  //connection
  for(i <- 0 until 12){
    BN_Array(i).io.input := io.from_PE(i)
    BN_Array(i).io.control := io.control(i)
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
    when(io.input < 0.U){
      io.output := 0.U
    }
    .elsewhen((io.input >= 0.U) && (io.input <= 6.U)){
      io.output := io.input
    }
    .otherwise{
      io.output := 6.U
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
  val Relu6_Array = Seq.fill(12)(Module(new BN_Unit(data_width = width)))

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


  val output_reg = RegInit(0.U(width.W))
  io.output := output_reg

  when(io.control === 0.U){
    output_reg := io.input
  }
  //sigmoid
  when(io.control === 1.U){
    output_reg := io.input
  }
  //tanh
  when(io.control === 2.U){
    output_reg := io.input
  }
  //relu6
  when(io.control === 3.U){
    output_reg := relu6.io.output
  }
}

class tanh_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(width.W))
    val output = Output(UInt(width.W))
  })

  //tanh
  io.output := io.input
}

class accumulator_registers(data_width: Int,addr_width:Int) extends Module {
  val io = IO(new Bundle {
    val rdAddr = Input(UInt(addr_width.W))
    val rdData = Output(UInt(data_width.W))
    val wrEna = Input(Bool())
    val wrData = Input(UInt(data_width.W))
    val wrAddr = Input(UInt(addr_width.W))
  })

  val register = RegInit(VecInit(Seq.fill(2^addr_width)(0.U(data_width.W))))

  io.rdData := register(io.rdAddr)
  when(io.wrEna){
    register(io.wrAddr) := io.wrData + register(io.wrAddr)
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
  printf("register63=%d\n",register(63))

  printf("\n")
  //to_PE
  when(io.to_PE_control <= 4.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(io.to_PE_control * 12.U + i.U)
    }
  }
  .elsewhen(io.to_PE_control === 5.U){
    for (i <- 0 until 4){
      io.to_PE(i) := register(io.to_PE_control * 12.U + i.U)
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
  when(io.wrEna){
    register(io.wrAddr) := io.wrData + register(io.wrAddr)
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

  val reg_Rt_Uhht_1 = RegInit(0.U(width.W))
  val reg_Whxt      = RegInit(0.U(width.W))
  val reg_Zt        = RegInit(0.U(width.W))
  val reg_ht_1      = RegInit(0.U(width.W))
  val reg_tanh      = RegInit(0.U(width.W))
  val reg_1_Zt      = RegInit(0.U(width.W))
  val reg_Zt_ht_1   = RegInit(0.U(width.W))
  val reg_output    = RegInit(0.U(width.W))

  reg_Rt_Uhht_1 := io.Rt_input * io.Uhht_1_input
  reg_Whxt      := io.Whxt_input
  reg_Zt        := io.Zt_input
  reg_ht_1      := io.ht_1_input

  val tanh = Module(new tanh_Unit(width = width))
  tanh.io.input := reg_Rt_Uhht_1 + reg_Whxt
  reg_tanh      := tanh.io.output
  reg_1_Zt      := 1.U - reg_Zt
  reg_Zt_ht_1   := reg_Zt * reg_ht_1

  reg_output  := reg_tanh * reg_1_Zt + reg_Zt_ht_1
  io.output   := reg_output
}
