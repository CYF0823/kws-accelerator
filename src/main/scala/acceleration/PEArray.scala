package acceleration

import chisel3._
import chisel3.util._

class PE_controller() extends Bundle(){
  val control = UInt(3.W)
  val mask    = UInt(12.W)
}

class PE(width: Int, row: Int, column: Int) extends Module {
  val io = IO(new Bundle {
    val FromAbovePE   = Input(UInt(width.W))
    val FromLeftPE    = Input(UInt(width.W))
    val FromL1        = Input(UInt(width.W))
    val control_signal= Input(new PE_controller())
    val ToRightPE     = Output(UInt(width.W))
    val ToBelowPE     = Output(UInt(width.W))
  })

  val sel1 = Wire(Bool())   //upper left
  val sel2 = Wire(Bool())   //bottom
  val sel3 = Wire(Bool())   //bottom right
  val sel4 = Wire(Bool())   //MAC input
  val sel5 = Wire(Bool())   //MAC logic

  val mux1out = Wire(UInt(width.W))
  when(sel1) {mux1out := io.FromL1}
    .otherwise{mux1out := io.FromLeftPE}

  val mux2out = Wire(UInt(width.W))
  when(sel2) {mux2out := MAC_out}
    .otherwise{mux2out := io.FromAbovePE}

  val L0Index = Wire(UInt(8.W))
  val L0Memory = Reg(Vec(100,UInt(width.W)))

  val mux3out = Wire(UInt(width.W))
  when(sel3) {mux3out := MAC_out}
    .otherwise{mux3out := mux1out}

  val mux4out = Wire(UInt(width.W))
  when(sel4) {mux4out := io.FromAbovePE}
    .otherwise{mux4out := 0.U(width.W)}

  val MAC_out = Wire(UInt(width.W))
  when(sel5)  {MAC_out := mux1out * mux4out + L0Memory(L0Index)}
    .otherwise{MAC_out := mux1out * L0Memory(L0Index) + mux4out}

  val mux2out_reg = RegNext(mux2out,0.U(width.W))
  val mux3out_reg = RegNext(mux3out,0.U(width.W))

  //output connection
  io.ToBelowPE := mux2out_reg
  io.ToRightPE := mux3out_reg

  //state_machine

  val idle :: dw1 :: pw1 ::  gru :: fc :: Nil = Enum(5)
  val state = RegInit(idle)
  val count = RegInit(0.U(10.W))

  //mask
  when (io.control_signal.mask(column) === 1.U){
    switch(io.control_signal.control) {
      //idle
      is(0.U(3.W)) {
        state := idle
      }
      //dw1
      is(1.U(3.W)) {
        state := dw1
      }
      //pw1
      is(2.U(3.W)) {
        state := pw1
      }
      //gru
      is(3.U(3.W)) {
        state := gru
      }
      //fc
      is(4.U(3.W)) {
        state := fc
      }
    }
  }

  switch(state) {
    is(idle) {

    }
    is(dw1) {
      sel1 := true.B  //upper left
      sel2 := true.B  //bottom
      sel4 := true.B  //MAC input
      sel5 := false.B  //MAC logic
      L0Index := row.U

      when(count =/= 51.U){
        count := count + 1
      }
      when(count === 51.U){
        count := 0.U
        state := idle
      }

    }
    is(pw1) {
      sel1 := false.B  //upper left
      sel2 := true.B  //bottom
      sel3 := false.B //bottom right
      sel4 := false.B  //MAC input
      sel5 := false.B  //MAC logic
      L0Index := column.U + 3.U

      when(count =/= 392.U){
        count := count + 1
      }
      when(count === 392.U){
        count := 0.U
        state := idle
      }
    }
    is(gru) {

    }
    is(fc) {

    }
  }
}

class BN_Unit(data_width:Int) extends Module{
  val io = IO(new Bundle{
    val from_PE   = Input(UInt(data_width.W))
    val control   = Input(UInt(2.W))
    val load_data = Input(UInt(data_width.W))
    val to_Relu6  = Output(UInt(data_width.W))
  })

  val BN_bias = RegInit(Vec(2,0.U))

  when(io.control === 0.U){
    BN_bias := io.load_data
  }

  when(io.control === 1.U){
    io.to_Relu6 := io.from_PE
  }
  when(io.control === 2.U){
    io.to_Relu6 := io.from_PE + BN_bias
  }
}

class BN_Unit_Array(width:Int) extends Module{
  val io = IO(new Bundle{
    val from_PE   = Input(Vec(12,UInt(width.W)))
    val control   = Input(Vec(12,UInt(2.W)))
    val load_data = Input(Vec(12,UInt(width.W)))
    val to_Relu6 = Output(Vec(12,UInt(width.W)))
  })

  val BN_Array = Vec(12,Module(new BN_Unit(data_width = width)))

  //connection
  for(i <- 0 until 12){
    BN_Array(i).io.from_PE := io.from_PE(i)
    BN_Array(i).io.control := io.control(i)
    BN_Array(i).io.load_data := io.load_data(i)
    io.to_Relu6(i) := BN_Array(i).io.to_Relu6
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
    val from_PE   = Input(UInt(width.W))
    val control   = Input(UInt(2.W))
    val to_partial_sum = Output(UInt(width.W))
  })

  val output_reg = RegInit(0.U(width.W))
  io.to_partial_sum := output_reg

  when(io.control === 0.U){
    output_reg := io.from_PE
  }
  //sigmoid
  when(io.control === 1.U){
    output_reg := io.from_PE
  }
  //tanh
  when(io.control === 2.U){
    output_reg := io.from_PE
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

  //val register = RegInit(Vec(2^addr_width,UInt(data_width.W)))
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

  val register = Reg(Vec(2^addr_width,UInt(data_width.W)))
  //val register = RegInit(Vec(Seq.fill(2^addr_width)(0.U(data_width.W))))

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


class PEArray(width: Int) extends Module {
  val io = IO(new Bundle {
    val From_L1_data    = Input(Vec(12,UInt(width.W)))
    val PE_control      = Input(Vec(3,new PE_controller()))
    val BN_control      = Input(Vec(12,UInt(2.W)))
    val Relu6_control   = Input(Vec(12,UInt(1.W)))
    val rd_data_mux     = Input(UInt(4.W))
    val To_L1_data      = Output(Vec(12,UInt(width.W)))
  })

  val PE_Array = for (i <- 0 until 12) yield{
    for (j <- 0 until 3) yield{
      val pe = Module(new PE(width = width, row = j, column = i))
      pe
    }
  }

  val BN_Array_below = Module(new BN_Unit_Array(width = width))
  val Relu6_Array = Module(new Relu6_Unit_Array(width = width))
  val BNUnit_right = Module(new BN_Unit(data_width = width))
  val activation = Module(new activation_Unit(width = width))

  val Zt = Module(new accumulator_registers(data_width = width,addr_width = 6))
  val Rt = Module(new accumulator_registers(data_width = width,addr_width = 6))
  val WhXt = Module(new accumulator_registers(data_width = width,addr_width = 6))
  val Uhht_1 = Module(new accumulator_registers(data_width = width,addr_width = 6))
  val Ht = Module(new ht(data_width = width,addr_width = 6))

  //control signal connection
  for (i <- 0 until 12){
    for (j <- 0 until 3){
      PE_Array(i)(j).io.control_signal := io.PE_control(j)
    }
  }
  BN_Array_below.io.control := io.BN_control
  Relu6_Array.io.control := io.Relu6_control

  //horizontal connection
  for (i <- 0 until 11){
    for(j <- 0 until 3){
      PE_Array(i+1)(j).io.FromLeftPE := PE_Array(i)(j).io.ToRightPE
    }
  }

  //vertical connection
  for (i <- 0 until 12){
    for(j <- 0 until 2){
      PE_Array(i)(j+1).io.FromAbovePE := PE_Array(i)(j).io.ToBelowPE
    }
  }

  //loop connection and to L1
  for (i <- 0 until 12){
    PE_Array(i)(0).io.FromAbovePE := PE_Array(i)(2).io.ToBelowPE
    BN_Array_below.io.from_PE(i) := PE_Array(i)(2).io.ToBelowPE
  }
  Relu6_Array.io.from_BN := BN_Array_below.io.to_Relu6
  io.To_L1_data := Relu6_Array.io.to_L1_Memory

  //from L1
  PE_Array(0)(0).io.FromL1 := io.From_L1_data(0)
  PE_Array(0)(1).io.FromL1 := io.From_L1_data(0)
  val rd_data_mux_delay = RegNext(io.rd_data_mux)
  PE_Array(0)(2).io.FromL1 := io.From_L1_data(rd_data_mux_delay)
  for (i <- 1 until 12){
    for(j <- 0 until 3){
      PE_Array(i)(j).io.FromL1 := io.From_L1_data(i)
    }
  }

  //horizontal to partial sum
  BNUnit_right.io.from_PE := PE_Array(11)(2).io.ToRightPE

}
