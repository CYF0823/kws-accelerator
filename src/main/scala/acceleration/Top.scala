package acceleration

import chisel3._

class Top(bit_width: Int, mem_addr_width: Int) extends Module {
  val io = IO(new Bundle {
  })
  val L1_Memory_top = Wire(Vec(12,Module(new Memory(data_width = bit_width,addr_width = mem_addr_width))))
  val PEArray_top = Module(new PEArray(bit_width))
  val FSM_top = Module(new FSM(n = bit_width,addr = mem_addr_width))

  //connection
  for (i <- 0 until 11){
    L1_Memory_top(i).io.rdAddr := FSM_top.io.L1_rd_addr(i)
    L1_Memory_top(i).io.wrAddr := FSM_top.io.L1_wr_addr(i)
    L1_Memory_top(i).io.wrEna  := FSM_top.io.L1_wrEna(i)
  }

  PEArray_top.io.PE_control := FSM_top.io.PEArray_ctrl
  PEArray_top.io.rd_data_mux := FSM_top.io.PE_rd_data_mux
  PEArray_top.io.BN_control := FSM_top.io.BNArray_ctrl
  PEArray_top.io.Relu6_control := FSM_top.io.Relu6_ctrl

}
