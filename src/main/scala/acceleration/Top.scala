package acceleration

import chisel3._

class Top(bit_width: Int, mem_addr_width: Int,registers_addr_width:Int) extends Module {
  val io = IO(new Bundle {
  })

  val L1_Memory_top = Wire(Vec(12,Module(new Memory(data_width = bit_width,addr_width = mem_addr_width))))
  val PEArray_top = Module(new PEArray(bit_width))
  val FSM_top = Module(new FSM(n = bit_width,addr = mem_addr_width))

  val BN_Array_below = Module(new BN_Unit_Array(width = bit_width))
  val Relu6_Array = Module(new Relu6_Unit_Array(width = bit_width))
  val BNUnit_right = Module(new BN_Unit(data_width = bit_width))
  val Activation = Module(new activation_Unit(width = bit_width))

  val Zt = Module(new accumulator_registers(data_width = bit_width,addr_width = 6))
  val Rt = Module(new accumulator_registers(data_width = bit_width,addr_width = 6))
  val WhXt = Module(new accumulator_registers(data_width = bit_width,addr_width = 6))
  val Uhht_1 = Module(new accumulator_registers(data_width = bit_width,addr_width = 6))
  val Ht = Module(new ht(data_width = bit_width,addr_width = 6))

  //Memory
  for (i <- 0 until 12){
    L1_Memory_top(i).io.rdAddr := FSM_top.io.L1_rd_addr(i)
    L1_Memory_top(i).io.wrAddr := FSM_top.io.L1_wr_addr(i)
    L1_Memory_top(i).io.wrEna  := FSM_top.io.L1_wrEna(i)
    L1_Memory_top(i).io.wrData := Relu6_Array.io.to_L1_Memory(i)
  }

  //PE Array
  PEArray_top.io.PE_control   := FSM_top.io.PEArray_ctrl
  PEArray_top.io.rd_data_mux  := FSM_top.io.PE_rd_data_mux

  when(FSM_top.io.PE_above_data_ctrl === 0.U){
    for(i <- 0 until 12){
      PEArray_top.io.From_above(i) := L1_Memory_top(i).io.rdData
    }
  }

  when(FSM_top.io.PE_above_data_ctrl === 1.U){
    PEArray_top.io.From_above := Ht.io.to_PE
  }

  //BN
  BN_Array_below.io.control := FSM_top.io.BNArray_ctrl
  BN_Array_below.io.from_PE := PEArray_top.io.To_below
  BNUnit_right.io.control   := FSM_top.io.BN_Unit_ctrl
  BNUnit_right.io.input     := PEArray_top.io.To_right(2)

  //Relu6
  Relu6_Array.io.control := FSM_top.io.Relu6Array_ctrl
  Relu6_Array.io.from_BN := BN_Array_below.io.to_Relu6

  //activation
  Activation.io.input   := BNUnit_right.io.output
  Activation.io.control := FSM_top.io.Activation_ctrl

  //ht and partial sum
  Ht.io.wrData        := Activation.io.output
  Ht.io.wrEna         := FSM_top.io.Ht_wrEna
  Ht.io.wrAddr        := FSM_top.io.Ht_wrAddr
  Ht.io.to_PE_control := FSM_top.io.Ht_to_PE_control

  Zt.io.wrData  := Activation.io.output
  Zt.io.wrEna   := FSM_top.io.Ht_wrEna
  Zt.io.wrAddr  := FSM_top.io.Ht_wrAddr

  Rt.io.wrData  := Activation.io.output
  Rt.io.wrEna   := FSM_top.io.Ht_wrEna
  Rt.io.wrAddr  := FSM_top.io.Ht_wrAddr

  WhXt.io.wrData  := Activation.io.output
  WhXt.io.wrEna   := FSM_top.io.Ht_wrEna
  WhXt.io.wrAddr  := FSM_top.io.Ht_wrAddr

  Uhht_1.io.wrData  := Activation.io.output
  Uhht_1.io.wrEna   := FSM_top.io.Ht_wrEna
  Uhht_1.io.wrAddr  := FSM_top.io.Ht_wrAddr
  
}
