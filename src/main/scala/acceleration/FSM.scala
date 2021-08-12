package acceleration

import chisel3._
import chisel3.util._

import scala.collection.immutable.Nil

class FSM(width: Int) extends Module {
  val io = IO(new Bundle {
    val Start               = Input(Bool())
    val Input_Data          = Input(UInt(width.W))
    val Input_Valid         = Input(Bool())
    val Input_Ready         = Output(Bool())
    val L1_wr_data          = Output(UInt(width.W))

    val To_L1_control       = Output(Bool())
    val L1_rd_addr          = Output(Vec(12, UInt(12.W)))
    val PE_rd_data_mux      = Output(UInt(4.W))
    val L1_wr_addr          = Output(Vec(12, UInt(12.W)))
    val L1_wrEna            = Output(Vec(12, Bool()))

    val PEArray_ctrl        = Output(Vec(3,new PE_controller()))
    val BNArray_ctrl        = Output(Vec(12,UInt(2.W)))
    val BNArray_wrAddr      = Output(UInt(2.W))
    val BNArray_wrEna       = Output(Bool())
    val BNArray_wrData      = Output(UInt(width.W))
    val BN_Unit_ctrl        = Output(UInt(2.W))
    val BN_Unit_wrAddr      = Output(UInt(2.W))
    val BN_Unit_wrEna       = Output(Bool())
    val BN_Unit_wrData      = Output(UInt(width.W))
    val Relu6Array_ctrl     = Output(Vec(12,UInt(1.W)))
    val PE_above_data_ctrl  = Output(UInt(2.W))
    val Activation_ctrl     = Output(UInt(2.W))

    val Ht_to_PE_control    = Output(UInt(3.W))
    val Ht_rdAddr           = Output(UInt(6.W))
    val Ht_wrEna            = Output(Bool())
    val Ht_wrAddr           = Output(UInt(6.W))
    val Zt_rdAddr           = Output(UInt(6.W))
    val Zt_wrEna            = Output(Bool())
    val Zt_wrAddr           = Output(UInt(6.W))
    val Rt_rdAddr           = Output(UInt(6.W))
    val Rt_wrEna            = Output(Bool())
    val Rt_wrAddr           = Output(UInt(6.W))
    val WhXt_rdAddr         = Output(UInt(6.W))
    val WhXt_wrEna          = Output(Bool())
    val WhXt_wrAddr         = Output(UInt(6.W))
    val Uhht_1_rdAddr       = Output(UInt(6.W))
    val Uhht_1_wrEna        = Output(Bool())
    val Uhht_1_wrAddr       = Output(UInt(6.W))
    val FC_temp_to_PE_control = Output(UInt(3.W))
    val FC_temp_rdAddr        = Output(UInt(6.W))
    val FC_temp_wrEna         = Output(Bool())
    val FC_temp_wrAddr        = Output(UInt(6.W))
    val Result_rdAddr         = Output(UInt(4.W))
    val Result_wrEna          = Output(Bool())
    val Result_wrAddr         = Output(UInt(4.W))
  })

  val L1_rd_addr      = Reg(Vec(12, UInt(12.W)))
  val PE_rd_data_mux  = RegInit(0.U(4.W))
  val L1_wr_addr      = Reg(Vec(12, UInt(12.W)))
  val L1_wrEna        = Reg(Vec(12, Bool()))
  val PEArray_ctrl    = Reg(Vec(3,new PE_controller()))
  val BNArray_ctrl    = Reg(Vec(12,UInt(2.W)))
  val BNArray_wrAddr  = Reg(UInt(2.W))
  val BNArray_wrEna   = Reg(Bool())
  val BNArray_wrData  = Reg(UInt(width.W))
  val BN_Unit_ctrl    = Reg(UInt(2.W))
  val BN_Unit_wrAddr  = Reg(UInt(2.W))
  val BN_Unit_wrEna   = Reg(Bool())
  val BN_Unit_wrData  = Reg(UInt(width.W))
  val Relu6Array_ctrl = Reg(Vec(12,UInt(1.W)))
  val PE_above_data_ctrl = RegInit(0.U(2.W))
  val Activation_ctrl = RegInit(0.U(2.W))

  val Ht_to_PE_control    = RegInit(0.U(3.W))
  val Ht_rdAddr           = RegInit(0.U(6.W))
  val Ht_wrEna            = RegInit(false.B)
  val Ht_wrAddr           = RegInit(0.U(6.W))
  val Zt_rdAddr           = RegInit(0.U(6.W))
  val Zt_wrEna            = RegInit(false.B)
  val Zt_wrAddr           = RegInit(0.U(6.W))
  val Rt_rdAddr           = RegInit(0.U(6.W))
  val Rt_wrEna            = RegInit(false.B)
  val Rt_wrAddr           = RegInit(0.U(6.W))
  val WhXt_rdAddr         = RegInit(0.U(6.W))
  val WhXt_wrEna          = RegInit(false.B)
  val WhXt_wrAddr         = RegInit(0.U(6.W))
  val Uhht_1_rdAddr       = RegInit(0.U(6.W))
  val Uhht_1_wrEna        = RegInit(false.B)
  val Uhht_1_wrAddr       = RegInit(0.U(6.W))

  val FC_temp_to_PE_control = RegInit(0.U(3.W))
  val FC_temp_rdAddr        = RegInit(0.U(6.W))
  val FC_temp_wrEna         = RegInit(false.B)
  val FC_temp_wrAddr        = RegInit(0.U(6.W))
  val Result_rdAddr         = RegInit(0.U(4.W))
  val Result_wrEna          = RegInit(false.B)
  val Result_wrAddr         = RegInit(0.U(4.W))

  //load
  val Ready_reg = RegInit(true.B)
  val Data_temp = Reg(UInt(width.W))
  val Data_temp_used = RegInit(true.B)
  val L1_wr_data     = Reg(UInt(width.W))
  val To_L1_control  = Reg(Bool())

  //output connection
  io.L1_rd_addr     := L1_rd_addr
  io.PE_rd_data_mux := PE_rd_data_mux
  io.L1_wr_addr     := L1_wr_addr
  io.L1_wrEna       := L1_wrEna
  io.PEArray_ctrl   := PEArray_ctrl
  io.BNArray_ctrl   := BNArray_ctrl
  io.BNArray_wrAddr := BNArray_wrAddr
  io.BNArray_wrEna  := BNArray_wrEna
  io.BNArray_wrData := BNArray_wrData
  io.BN_Unit_ctrl   := BN_Unit_ctrl
  io.BN_Unit_wrAddr := BN_Unit_wrAddr
  io.BN_Unit_wrEna  := BN_Unit_wrEna
  io.BN_Unit_wrData := BN_Unit_wrData
  io.Relu6Array_ctrl:= Relu6Array_ctrl
  io.PE_above_data_ctrl := PE_above_data_ctrl
  io.Activation_ctrl := Activation_ctrl

  io.Ht_to_PE_control := Ht_to_PE_control
  io.Ht_rdAddr        := Ht_rdAddr
  io.Ht_wrEna         := Ht_wrEna
  io.Ht_wrAddr        := Ht_wrAddr
  io.Zt_rdAddr        := Zt_rdAddr
  io.Zt_wrEna         := Zt_wrEna
  io.Zt_wrAddr        := Zt_wrAddr
  io.Rt_rdAddr        := Rt_rdAddr
  io.Rt_wrEna         := Rt_wrEna
  io.Rt_wrAddr        := Rt_wrAddr
  io.WhXt_rdAddr      := WhXt_rdAddr
  io.WhXt_wrEna       := WhXt_wrEna
  io.WhXt_wrAddr      := WhXt_wrAddr
  io.Uhht_1_rdAddr    := Uhht_1_rdAddr
  io.Uhht_1_wrEna     := Uhht_1_wrEna
  io.Uhht_1_wrAddr    := Uhht_1_wrAddr

  io.FC_temp_to_PE_control := FC_temp_to_PE_control
  io.FC_temp_rdAddr        := FC_temp_rdAddr
  io.FC_temp_wrEna         := FC_temp_wrEna
  io.FC_temp_wrAddr        := FC_temp_wrAddr
  io.Result_rdAddr         := Result_rdAddr
  io.Result_wrEna          := Result_wrEna
  io.Result_wrAddr         := Result_wrAddr

  io.Input_Ready := Ready_reg
  io.L1_wr_data    := L1_wr_data
  io.To_L1_control := To_L1_control

  //state machine
  val idle :: load :: dw1 :: pw1 :: gru :: fc :: Nil = Enum(6)

  val state = RegInit(idle)
  val gru_state = RegInit(0.U(4.W))
  val count = RegInit(0.U(10.W))
  val count1 = RegInit(0.U(7.W))  //0-127
  val count2 = RegInit(0.U(7.W))
  val gru_count = RegInit(0.U(6.W))
  val read_index = RegInit(0.U(4.W))
  val fc_state = RegInit(0.U(2.W))

  switch(state) {
    is(idle) {
      when(io.Start === true.B){
        state := load
      }
    }
    is(load) {
      when(count === 0.U){
        Ready_reg := true.B
        io.To_L1_control := true.B
      }
      when(Data_temp_used === false.B){
        Data_temp_used := true.B
        //initialize
        when(count === 1.U){
          L1_wr_data := Data_temp
          for(i <- 0 until 12){
            L1_wr_addr(i) := 440.U
            L1_wrEna(i) := true.B
          }
        }
        when(count >= 2.U && count <= 15.U){
          L1_wr_data := Data_temp
          for(i <- 0 until 12){
            L1_wr_addr(i) := L1_wr_addr(i) + 1.U
            L1_wrEna(i) := true.B
          }
        }
      } .otherwise{//Data_temp_used === true.B
        for(i <- 0 until 12){
          L1_wrEna(i) := false.B
        }
      }

      when(io.Input_Valid === true.B && io.Input_Ready === true.B){
        Data_temp := io.Input_Data
        Data_temp_used := false.B
        count := count + 1.U
      }
      when(count === 16.U){
        state := dw1
        io.To_L1_control := false.B
      }

    }
    is(dw1) {
      when(count =/= 52.U){
        count := count + 1.U
      }
      when(count1 =/= 9.U){
        count1 := count1 + 1.U
      }.otherwise{
        count1 := 0.U
      }
      //PE state
      when(count === 0.U){
        for (i <- 0 until 3){
          PEArray_ctrl(i).control := 1.U
          PEArray_ctrl(i).mask := "b1111_1111_1111".U
        }
        //read initialize
        for (i <- 0 until 12) {
          L1_rd_addr(i) := 0.U
        }
        PE_above_data_ctrl := 0.U
        PE_rd_data_mux := 0.U
      }
      when(count === 1.U){
        for (i <- 0 until 3){
          PEArray_ctrl(i).mask := "b0000_0000_0000".U
        }
      }
      //read
      when((count >= 1.U) && (count <= 49.U)){
        for (i <- 0 until 12) {
          L1_rd_addr(i) := L1_rd_addr(i) + 1.U
        }
      }
      //write initialize
      when(count === 4.U){
        for (i <- 0 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := 400.U
          BNArray_ctrl(i) := 0.U
          Relu6Array_ctrl(i) := 0.U
        }
      }
      //write
      when((count >= 5.U) && (count <= 43.U)){
        when((count1 === 2.U) && (count1 === 3.U)){ //(count % 10.U === 2.U) && (count % 10.U === 3.U)
          for (i <- 0 until 12) {
            L1_wrEna(i) := false.B
          }
        }.otherwise{  //(count % 10.U =/= 2.U) && (count % 10.U =/= 3.U)
          for (i <- 0 until 12) {
            L1_wrEna(i) := true.B
            L1_wr_addr(i) := L1_wr_addr(i) + 1.U
          }
        }
      }
      when((count >= 44.U) && (count <= 51.U)){
        L1_wrEna(0) := true.B
        L1_wr_addr(0) := L1_wr_addr(0) + 1.U
        for (i <- 1 until 12) {
          L1_wrEna(i) := false.B
        }
      }
      //next state
      when(count === 52.U){
        for (i <- 0 until 12) {
          L1_wrEna(i) := false.B
        }
        count := 0.U
        count1 := 0.U
        state := pw1
      }
    }

    is(pw1) {
      when(count =/= 405.U){
        count := count + 1.U
      }
      //%8
      when(count1 =/= 7.U){
        count1 := count1 + 1.U
      }.otherwise{
        count1 := 0.U
      }
      //%96
      when(count2 =/= 95.U){
        count1 := count1 + 1.U
      }.otherwise{
        count1 := 0.U
      }

      when(count === 0.U){
        //PE state
        PEArray_ctrl(2).control := 2.U
        PEArray_ctrl(2).mask := "b1000_0000_0000".U
        //read initialize
        for (i <- 0 until 12) {
          L1_rd_addr(i) := 400.U
        }
        PE_above_data_ctrl := 0.U
        PE_rd_data_mux := 0.U
        //write initialize
        for (i <- 0 until 12) {
          L1_wr_addr(i) := 0.U
          BNArray_ctrl(i) := 1.U
          Relu6Array_ctrl(i) := 1.U
        }
      }

      //PE state
      when((count >= 1.U) && (count <= 11.U)){
        PEArray_ctrl(2).control := 2.U
        PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
      }
      when(count === 12.U){
        for (i <- 0 until 3){
          PEArray_ctrl(i).mask := "b0000_0000_0000".U
        }
      }

      //read
      when(count >= 1.U && count <= 391.U){
        L1_rd_addr(read_index) := L1_rd_addr(read_index) + 1.U
        when((count1 === 7.U) && (count2 =/= 95.U)){
          read_index := read_index + 1.U
          PE_rd_data_mux := PE_rd_data_mux + 1.U
        }
        when(count2 === 95.U){
          read_index := 0.U
        }
      }

      //write
      when(count === 2.U){
        for (i <- 0 until 1) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 3.U){
        for (i <- 0 until 2) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 4.U){
        for (i <- 0 until 3) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 5.U){
        for (i <- 0 until 4) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 6.U){
        for (i <- 0 until 5) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 7.U){
        for (i <- 0 until 6) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 8.U){
        for (i <- 0 until 7) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 9.U){
        for (i <- 0 until 8) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 10.U){
        for (i <- 0 until 9) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 11.U){
        for (i <- 0 until 10) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 12.U){(count >= 44.U) && (count <= 51.U)
        for (i <- 0 until 11) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when((count >= 13.U) && (count <= 393.U)){
        for (i <- 0 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 394.U){
        for (i <- 1 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 395.U){
        for (i <- 2 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 396.U){
        for (i <- 3 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 397.U){
        for (i <- 4 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 398.U){
        for (i <- 5 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 399.U){
        for (i <- 6 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 400.U){
        for (i <- 7 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 401.U){
        for (i <- 8 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 402.U){
        for (i <- 9 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 403.U){
        for (i <- 10 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      when(count === 404.U){
        for (i <- 11 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1.U
        }
      }
      //next state
      when(count === 405.U){
        for (i <- 0 until 12) {
          L1_wrEna(i) := false.B
        }
        count := 0.U
        count1 := 0.U
        count2 := 0.U
        state := gru
      }
    }

    is(gru) {
      //load ht and input to PE
      when(gru_state === 0.U){
        when(count =/= 18.U){
          count := count + 1.U
        }
        //load ht
        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 3.U
          PEArray_ctrl(2).mask := "b1111_1111_1111".U
          PEArray_ctrl(2).count := 15.U
          PEArray_ctrl(2).L0index := 15.U
          //load ht initialize
          PE_above_data_ctrl := 1.U
          PE_rd_data_mux := 0.U
          }
        when(count === 1.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }
        when(count === 2.U){
          Ht_to_PE_control := 0.U
        }
        //load ht
        when((count >= 3.U) && (count <= 7.U)){
          Ht_to_PE_control := Ht_to_PE_control + 1.U
        }
        //load input initialize
        when(count === 7.U){
          PE_above_data_ctrl := 0.U
          for(i <- 0 until 12){
            L1_rd_addr(i) := 0.U
          }
        }
        //load input
        when((count >= 8.U) && (count <= 14.U)){
          for(i <- 0 until 12){
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 18.U){
          count := 0.U
          gru_state := 1.U
        }
      }
      //zt
      when(gru_state === 1.U){
        when(count =/= 910.U){
          count := count + 1.U
        }
        //%64
        when(count1 =/= 63.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 895.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 64.U

          //read initialize
          for (i <- 0 until 12) {
            L1_rd_addr(i) := 500.U
          }
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          Zt_wrAddr := 0.U
          BN_Unit_ctrl := 1.U //bz
          Activation_ctrl := 1.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 895.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 64.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 895.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 896.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 897.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 898.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 899.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 900.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 901.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 902.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 903.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 904.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 905.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 906.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 909.U)){
          when(count1 === 14.U) {
            Zt_wrAddr := 0.U
            Zt_wrEna := true.B
          }
          .otherwise{
            Zt_wrAddr := Zt_wrAddr + 1.U
            Zt_wrEna := true.B
          }
        }
        //next state
        when(count === 910.U){
          count := 0.U
          count1 := 0.U
          Zt_wrEna := false.B
          gru_state := 2.U
        }
      }
      //rt
      when(gru_state === 2.U){
        when(count =/= 910.U){
          count := count + 1.U
        }
        //%64
        when(count1 =/= 63.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 895.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 64.U

          //read initialize
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          Rt_wrAddr := 0.U
          BN_Unit_ctrl := 2.U //br
          Activation_ctrl := 1.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 895.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 64.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 895.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 896.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 897.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 898.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 899.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 900.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 901.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 902.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 903.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 904.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 905.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 906.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 909.U)){
          when(count1 === 14.U) {
            Rt_wrAddr := 0.U
            Rt_wrEna := true.B
          }
            .otherwise{
              Rt_wrAddr := Rt_wrAddr + 1.U
              Rt_wrEna := true.B
            }
        }
        //next state
        when(count === 910.U){
          count := 0.U
          count1 := 0.U
          Rt_wrEna := false.B
          gru_state := 3.U
        }
      }

      //WhXt
      when(gru_state === 3.U){
        when(count =/= 526.U){
          count := count + 1.U
        }
        //%64
        when(count1 =/= 63.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 511.U
          PEArray_ctrl(2).L0index := 21.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 64.U

          //read initialize
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          WhXt_wrAddr := 0.U
          BN_Unit_ctrl := 0.U
          Activation_ctrl := 0.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 511.U
          PEArray_ctrl(2).L0index := 21.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 64.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 511.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 512.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 513.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 514.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 515.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 516.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 517.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 518.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 519.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 520.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 521.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 522.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 525.U)){
          when(count1 === 14.U) {
            WhXt_wrAddr := 0.U
            WhXt_wrEna := true.B
          }
            .otherwise{
              WhXt_wrAddr := WhXt_wrAddr + 1.U
              WhXt_wrEna := true.B
            }
        }
        //next state
        when(count === 526.U){
          count := 0.U
          count1 := 0.U
          WhXt_wrEna := false.B
          gru_state := 4.U
        }

      }
      //Uhht_1
      when(gru_state === 4.U){
        when(count =/= 398.U){
          count := count + 1.U
        }

        //%64
        when(count1 =/= 63.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 383.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 64.U

          //read initialize
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          Uhht_1_wrAddr := 0.U
          BN_Unit_ctrl := 0.U
          Activation_ctrl := 0.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 383.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 64.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 383.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 384.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 385.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 386.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 387.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 388.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 389.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 390.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 391.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 392.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 393.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 394.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 397.U)){
          when(count1 === 14.U) {
            Uhht_1_wrAddr := 0.U
            Uhht_1_wrEna := true.B
          }
            .otherwise{
              Uhht_1_wrAddr := Uhht_1_wrAddr + 1.U
              Uhht_1_wrEna := true.B
            }
        }
        //next state
        when(count === 398.U){
          count := 0.U
          count1 := 0.U
          Uhht_1_wrEna := false.B
          gru_state := 5.U
        }
      }
      //EW Unit
      when(gru_state === 5.U){
        when(count =/= 68.U){
          count := count + 1.U
        }
        //read initialize
        when(count === 0.U){
          Zt_rdAddr := 0.U
          Rt_rdAddr := 0.U
          WhXt_rdAddr := 0.U
          Uhht_1_rdAddr := 0.U
        }
        when((count >= 1.U) && (count <= 63.U)){
          Zt_rdAddr := Zt_rdAddr + 1.U
          Rt_rdAddr := Rt_rdAddr + 1.U
          WhXt_rdAddr := WhXt_rdAddr + 1.U
          Uhht_1_rdAddr := Uhht_1_rdAddr + 1.U
        }
        //write initialize
        when(count === 3.U){
          Ht_wrAddr := 0.U
          Ht_wrEna := true.B
        }
        when(count <= 4.U && count >= 67.U){
          Ht_wrAddr := Ht_wrAddr + 1.U
        }
        when(count === 68.U){
          count := 0.U
          Ht_wrEna := false.B
          gru_state := 0.U
          when(gru_count === 63.U){
            state := fc
            gru_count := 0.U
          }.otherwise{
            state := gru
            gru_count := gru_count + 1.U
            }
        }
      }
    }

    is(fc) {
      when(fc_state === 0.U){
        when(count =/= 10.U){
          count := count + 1.U
        }
        //load ht
        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 3.U
          PEArray_ctrl(2).mask := "b1111_1111_1111".U
          PEArray_ctrl(2).count := 7.U
          PEArray_ctrl(2).L0index := 15.U
          //load ht initialize
          PE_above_data_ctrl := 1.U
          PE_rd_data_mux := 0.U
        }
        when(count === 1.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }
        when(count === 2.U){
          Ht_to_PE_control := 0.U
        }
        //load ht
        when((count >= 3.U) && (count <= 7.U)){
          Ht_to_PE_control := Ht_to_PE_control + 1.U
        }
        when(count === 10.U){
          count := 0.U
          fc_state := 1.U
        }
      }
      when(fc_state === 1.U){
        when(count =/= 398.U){
          count := count + 1.U
        }

        //%64
        when(count1 =/= 63.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 383.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 64.U

          //read initialize
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          FC_temp_wrAddr := 0.U
          BN_Unit_ctrl := 3.U //fc_BN_bias
          Activation_ctrl := 3.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 383.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 64.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 383.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 384.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 385.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 386.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 387.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 388.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 389.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 390.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 391.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 392.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 393.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 394.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 397.U)){
          when(count1 === 14.U) {
            FC_temp_wrAddr := 0.U
            FC_temp_wrEna := true.B
          }
            .otherwise{
              FC_temp_wrAddr := FC_temp_wrAddr + 1.U
              FC_temp_wrEna := true.B
            }
        }
        //next state
        when(count === 398.U){
          count := 0.U
          count1 := 0.U
          FC_temp_wrEna := false.B
          fc_state := 2.U
        }
      }
      when(fc_state === 2.U){
        when(count =/= 10.U){
          count := count + 1.U
        }
        //load ht
        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 3.U
          PEArray_ctrl(2).mask := "b1111_1111_1111".U
          PEArray_ctrl(2).count := 7.U
          PEArray_ctrl(2).L0index := 15.U
          //load ht initialize
          PE_above_data_ctrl := 2.U //choose FC_temp
          PE_rd_data_mux := 0.U
        }
        when(count === 1.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }
        when(count === 2.U){
          FC_temp_to_PE_control := 0.U
        }
        //load ht
        when((count >= 3.U) && (count <= 7.U)){
          FC_temp_to_PE_control := FC_temp_to_PE_control + 1.U
        }
        when(count === 10.U){
          count := 0.U
          fc_state := 3.U
        }
      }
      //Result
      when(fc_state === 3.U){
        when(count =/= 398.U){
          count := count + 1.U
        }
        //%12
        when(count1 =/= 11.U){
          count1 := count1 + 1.U
        }.otherwise{
          count1 := 0.U
        }

        when(count === 0.U){
          //PE state
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 71.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := "b1000_0000_0000".U
          PEArray_ctrl(2).gru_out_width := 12.U

          //read initialize
          PE_above_data_ctrl := 0.U
          PE_rd_data_mux := 0.U

          //write initialize
          FC_temp_wrAddr := 0.U
          BN_Unit_ctrl := 0.U //fc_BN_bias
          Activation_ctrl := 0.U
        }

        //PE state
        when((count >= 1.U) && (count <= 11.U)){
          PEArray_ctrl(2).control := 4.U
          PEArray_ctrl(2).count := 71.U
          PEArray_ctrl(2).L0index := 15.U
          PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
          PEArray_ctrl(2).gru_out_width := 12.U
        }
        when(count === 12.U){
          for (i <- 0 until 3){
            PEArray_ctrl(i).mask := "b0000_0000_0000".U
          }
        }

        //read
        when(count === 1.U){
          for (i <- 0 until 1) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 2.U){
          for (i <- 0 until 2) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 3.U){
          for (i <- 0 until 3) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 4.U){
          for (i <- 0 until 4) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 5.U){
          for (i <- 0 until 5) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 6.U){
          for (i <- 0 until 6) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 7.U){
          for (i <- 0 until 7) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 8.U){
          for (i <- 0 until 8) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 9.U){
          for (i <- 0 until 9) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 10.U){
          for (i <- 0 until 10) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 11.U){
          for (i <- 0 until 11) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when((count >= 12.U) && (count <= 71.U)){
          for (i <- 0 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 72.U){
          for (i <- 1 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 73.U){
          for (i <- 2 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 74.U){
          for (i <- 3 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 75.U){
          for (i <- 4 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 76.U){
          for (i <- 5 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 77.U){
          for (i <- 6 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 78.U){
          for (i <- 7 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 79.U){
          for (i <- 8 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 80.U){
          for (i <- 9 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 81.U){
          for (i <- 10 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        when(count === 82.U){
          for (i <- 11 until 12) {
            L1_rd_addr(i) := L1_rd_addr(i) + 1.U
          }
        }
        //write
        when((count >= 14.U) && (count <= 85.U)){
          when(count1 === 2.U) {
            Result_wrAddr := 0.U
            Result_wrEna := true.B
          }
            .otherwise{
              Result_wrAddr := Result_wrAddr + 1.U
              Result_wrEna := true.B
            }
        }
        //next state
        when(count === 86.U){
          count := 0.U
          count1 := 0.U
          Result_wrEna := false.B
          fc_state := 0.U
          state := idle
        }
      }


    }
  }
}


