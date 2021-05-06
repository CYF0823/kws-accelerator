package acceleration
import chisel3._

class BN_Unit(data_width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(data_width.W))
    val control   = Input(UInt(2.W))
    val output  = Output(UInt(data_width.W))
  })

  val BN_bias = RegInit(Vec(2,0.U))

  when(io.control === 0.U){
    BN_bias := io.input
  }

  when(io.control === 1.U){
    io.output := io.input
  }
  when(io.control === 2.U){
    io.output := io.input + BN_bias
  }
}

class BN_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_PE   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(2.W)))
    val to_Relu6 = Output(Vec(12,UInt(width.W)))
  })

  val BN_Array = Vec(12,Module(new BN_Unit(data_width = width)))

  //connection
  for(i <- 0 until 12){
    BN_Array(i).io.input := io.from_PE(i)
    BN_Array(i).io.control := io.control(i)
    io.to_Relu6(i) := BN_Array(i).io.output
  }

}

class Relu6_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_BN   = Input(UInt(width.W))
    val control   = Input(UInt(1.W))
    val to_L1_Memory = Output(UInt(width.W))
  })

  when(io.control === 0.U){
    io.to_L1_Memory := io.from_BN
  }
  when(io.control === 1.U){
    when(io.from_BN < 0.U){
      io.to_L1_Memory := 0.U
    }
    when((io.from_BN >= 0.U) && (io.from_BN <= 6.U)){
      io.to_L1_Memory := io.from_BN
    }
    when(io.from_BN > 6.U){
      io.to_L1_Memory := 6.U
    }
  }
}

class Relu6_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_BN   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(1.W)))
    val to_L1_Memory = Output(Vec(12,UInt(width.W)))
  })

  val Relu6_Array = Vec(12,Module(new Relu6_Unit(width = width)))

  //connection
  for(i <- 0 until 12){
    Relu6_Array(i).io.from_BN := io.from_BN(i)
    Relu6_Array(i).io.control := io.control(i)
    io.to_L1_Memory(i) := Relu6_Array(i).io.to_L1_Memory
  }

}

class activation_Unit(width:Int) extends Module{
  val io = IO(new Bundle{
    val input   = Input(UInt(width.W))
    val control   = Input(UInt(2.W))
    val output = Output(UInt(width.W))
  })

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
    val wrEna = Input(Bool())
    val wrData = Input(UInt(data_width.W))
    val wrAddr = Input(UInt(addr_width.W))
  })

  val register = RegInit(VecInit(Seq.fill(2^addr_width)(0.U(data_width.W))))

  //read
  when(io.to_PE_control <= 4.U){
    for (i <- 0 until 12){
      io.to_PE(i) := register(io.to_PE_control * 12.U + i.U)
    }
  }
  when(io.to_PE_control === 5.U){
    for (i <- 0 until 4){
      io.to_PE(i) := register(io.to_PE_control * 12.U + i.U)
    }
    for (i <- 4 until 12){
      io.to_PE(i) := 0.U
    }
  }
  //write
  when(io.wrEna){
    register(io.wrAddr) := io.wrData + register(io.wrAddr)
  }
}
