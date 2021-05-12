package acceleration

import chisel3._
import chisel3.util._

class PE_controller() extends Bundle(){
  val control = UInt(3.W)
  val count   = UInt(10.W)
  val L0index = UInt(6.W)
  val mask    = UInt(12.W)
  val gru_out_width = UInt(8.W)
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
  val sel4 = Wire(UInt(2.W))   //MAC input above
  val sel5 = Wire(Bool())   //MAC logic
  val sel6 = Wire(Bool())   //MAC input left

  //initialize
  sel1 := true.B
  sel2 := true.B
  sel3 := true.B
  sel4 := 0.U
  sel5 := true.B
  sel6 := true.B

  val L0Index = RegInit(0.U(6.W))
  val L0Memory = Reg(Vec(100,UInt(width.W)))


  val mux1out = Wire(UInt(width.W))
  val mux2out = Wire(UInt(width.W))
  val mux3out = Wire(UInt(width.W))
  val mux4out = Wire(UInt(width.W))
  val MAC_out = Wire(UInt(width.W))
  val mux6out = Wire(UInt(width.W))

  when(sel1) {mux1out := io.FromL1}
    .otherwise{mux1out := io.FromLeftPE}

  when(sel2) {mux2out := MAC_out}
    .otherwise{mux2out := io.FromAbovePE}

  when(sel3) {mux3out := MAC_out}
    .otherwise{mux3out := mux1out}

  when(sel4 === 0.U) {
    mux4out := 0.U(width.W)
  }
    .elsewhen(sel4 === 1.U){
      mux4out := io.FromAbovePE
    }
    .elsewhen(sel4 === 2.U){
      mux4out := io.FromL1
    }
    .otherwise{
      mux4out := 0.U(width.W)
    }

  when(sel5)  {MAC_out := mux6out * mux4out + L0Memory(L0Index)}
    .otherwise{MAC_out := mux6out * L0Memory(L0Index) + mux4out}

  when(sel6) {mux6out := mux1out}
    .otherwise{mux6out := 0.U(width.W)}

  val mux2out_reg = RegNext(mux2out,0.U(width.W))
  val mux3out_reg = RegNext(mux3out,0.U(width.W))

  //output connection
  io.ToBelowPE := mux2out_reg
  io.ToRightPE := mux3out_reg

  //state_machine

  val idle :: dw1 :: pw1 :: l0_load :: gru :: fc :: Nil = Enum(6)
  val state = RegInit(idle)
  val count = RegInit(0.U(10.W))
  val count_max = Reg(UInt(10.W))
  val L0index_begin = Reg(UInt(6.W))
  val GRU_out_width = Reg(UInt(6.W))

  switch(state) {
    is(idle) {
      sel1 := true.B
      sel2 := true.B
      sel3 := true.B
      sel4 := 0.U
      sel5 := false.B
      sel6 := true.B

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
          //l0_load
          is(3.U(3.W)) {
            state := l0_load
          }
          //gru
          is(4.U(3.W)) {
            state := gru
          }
          //fc
          is(5.U(3.W)) {
            state := fc
          }
        }
        count_max := io.control_signal.count
        L0index_begin := io.control_signal.L0index
        GRU_out_width := io.control_signal.gru_out_width
      }
    }
    is(dw1) {
      sel1 := true.B  //upper left
      sel2 := true.B  //bottom
      //MAC input above
      when(row.U === 0.U){
        sel4 := 0.U
      } otherwise {
        sel4 := 1.U
      }
      sel5 := false.B  //MAC logic
      sel6 := true.B
      L0Index := row.U

      when(count =/= 51.U){
        count := count + 1.U
      }
      when(count === 51.U){
        count := 0.U
        state := idle
      }

    }
    is(pw1) {
      when((row.U === 2.U) && (column.U === 0.U)){
        sel1 := true.B  //upper left
      } .otherwise {
        sel1 := false.B  //upper left
      }
      sel2 := true.B  //bottom
      sel3 := false.B //bottom right
      sel4 := 0.U  //MAC input
      sel5 := false.B  //MAC logic
      sel6 := true.B   //MAC input
      L0Index := column.U +& 3.U

      when(count =/= 392.U){
        count := count + 1.U
      }
      when(count === 392.U){
        count := 0.U
        state := idle
      }
    }
    is(l0_load) {
      when(count =/= count_max){
        count := count + 1.U
      }
      when(count === 0.U){
        L0Index := L0index_begin
      }
      when((count >= 1.U) && (count <= (count_max - 1.U))){
        L0Memory(L0Index) := io.FromL1
        L0Index := L0Index + 1.U
      }
      when(count === count_max){
        count := 0.U
        state := idle
      }
    }
    is(gru) {
      sel1 := false.B  //upper left
      sel3 := true.B //bottom right
      sel4 := 2.U  //MAC input
      sel5 := false.B  //MAC logic
      when((column.U === 0.U) && (row.U === 2.U)) {sel6 := false.B}
      .otherwise{sel6 := true.B}
      L0Index := L0index_begin

      when(count =/= count_max){
        count := count + 1.U
      }

      when((count % GRU_out_width) === (GRU_out_width - 1.U)){
        L0Index := L0Index + 1.U
      }

      when(count === count_max){
        count := 0.U
        state := idle
      }

    }
    is(fc) {
      sel1 := false.B  //upper left
      sel3 := true.B //bottom right
      sel4 := 2.U  //MAC input
      sel5 := false.B  //MAC logic
      when((column.U === 0.U) && (row.U === 2.U)) {sel6 := false.B}
        .otherwise{sel6 := true.B}
      L0Index := L0index_begin

      when(count =/= count_max){
        count := count + 1.U
      }

      when((count % GRU_out_width) === (GRU_out_width - 1.U)){
        L0Index := L0Index + 1.U
      }

      when(count === count_max){
        count := 0.U
        state := idle
      }
    }
  }
}

class PEArray(width: Int) extends Module {
  val io = IO(new Bundle {
    val From_above    = Input(Vec(12,UInt(width.W)))
    val PE_control    = Input(Vec(3,new PE_controller()))
    val rd_data_mux   = Input(UInt(4.W))
    val To_below      = Output(Vec(12,UInt(width.W)))
    val To_right      = Output(Vec(3,UInt(width.W)))
  })

  val PE_Array = for (i <- 0 until 12) yield{
    for (j <- 0 until 3) yield{
      val pe = Module(new PE(width = width, row = j, column = i))
      pe
    }
  }

  //control signal connection
  for (i <- 0 until 12){
    for (j <- 0 until 3){
      PE_Array(i)(j).io.control_signal := io.PE_control(j)
    }
  }

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
    io.To_below(i) := PE_Array(i)(2).io.ToBelowPE
  }

  //from L1
  PE_Array(0)(0).io.FromL1 := io.From_above(0)
  PE_Array(0)(1).io.FromL1 := io.From_above(0)
  val rd_data_mux_delay = RegNext(io.rd_data_mux)
  PE_Array(0)(2).io.FromL1 := io.From_above(rd_data_mux_delay)
  for (i <- 1 until 12){
    for(j <- 0 until 3){
      PE_Array(i)(j).io.FromL1 := io.From_above(i)
    }
  }

  //horizontal to partial sum
  for(j <- 0 until 3){
    io.To_right(j) := PE_Array(11)(j).io.ToRightPE
  }

}
