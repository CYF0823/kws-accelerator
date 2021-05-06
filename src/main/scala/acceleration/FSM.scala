package acceleration

import chisel3._
import chisel3.util._

import scala.collection.immutable.Nil

class FSM(n: Int,addr: Int) extends Module {
  val io = IO(new Bundle {
    val L1_rd_addr          = Output(Vec(12, UInt(addr.W)))
    val PE_rd_data_mux      = Output(UInt(4.W))
    val L1_wr_addr          = Output(Vec(12, UInt(addr.W)))
    val L1_wrEna            = Output(Vec(12, Bool()))
    val PEArray_ctrl        = Output(Vec(3,new PE_controller()))
    val BNArray_ctrl        = Output(Vec(12,UInt(2.W)))
    val BN_Unit_ctrl        = Output(UInt(2.W))
    val Relu6Array_ctrl     = Output(Vec(12,UInt(1.W)))
    val PE_above_data_ctrl  = Output(UInt(1.W))
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
  })

  val L1_rd_addr      = Reg(Vec(12, UInt(addr.W)))
  val PE_rd_data_mux  = RegInit(0.U(4.W))
  val L1_wr_addr      = Reg(Vec(12, UInt(addr.W)))
  val L1_wrEna        = Reg(Vec(12, Bool()))
  val PEArray_ctrl    = Reg(Vec(3,new PE_controller()))
  val BNArray_ctrl    = Reg(Vec(12,UInt(2.W)))
  val BN_Unit_ctrl    = Reg(UInt(2.W))
  val Relu6Array_ctrl = Reg(Vec(12,UInt(1.W)))
  val PE_above_data_ctrl = RegInit(0.U(1.W))
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

  //output connection
  io.L1_rd_addr     := L1_rd_addr
  io.PE_rd_data_mux := PE_rd_data_mux
  io.L1_wr_addr     := L1_wr_addr
  io.L1_wrEna       := L1_wrEna
  io.PEArray_ctrl   := PEArray_ctrl
  io.BNArray_ctrl   := BNArray_ctrl
  io.BN_Unit_ctrl   := BN_Unit_ctrl
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

  val idle :: dw1 :: pw1 :: gru :: fc :: Nil = Enum(5)

  val state = RegInit(idle)
  val count = RegInit(0.U(10.W))
  val read_index = RegInit(0.U(4.W))

  switch(state) {
    is(idle) {

    }

    is(dw1) {
      when(count =/= 52.U){
        count := count + 1
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
      //read
      when((count >= 1.U) && (count <= 49.U)){
        for (i <- 0 until 12) {
          L1_rd_addr(i) := L1_rd_addr(i) + 1
        }
      }
      //write initialize
      when(count === 4.U){
        for (i <- 0 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := 500.U
          BNArray_ctrl(i) := 1.U
          Relu6Array_ctrl(i) := 0.U
        }
      }
      //write
      when((count >= 5.U) && (count <= 43.U) && (count % 10.U =/= 2.U) && (count % 10.U =/= 3.U)){
        for (i <- 0 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when((count >= 5.U) && (count <= 43.U) && (count % 10.U === 2.U) && (count % 10.U === 3.U)){
        for (i <- 0 until 12) {
          L1_wrEna(i) := false.B
        }
      }
      when((count >= 44.U) && (count <= 51.U)){
        L1_wrEna(0) := true.B
        L1_wr_addr(0) := L1_wr_addr(0) + 1
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
        state := pw1
      }
    }

    is(pw1) {
      when(count =/= 405.U){
        count := count + 1
      }
      when(count === 0.U){
        //PE state
        PEArray_ctrl(2).control := 2.U
        PEArray_ctrl(2).mask := "b1000_0000_0000".U
        //read initialize
        for (i <- 0 until 12) {
          L1_rd_addr(i) := 500.U
        }
        PE_above_data_ctrl := 0.U
        PE_rd_data_mux := 0.U
        //write initialize
        for (i <- 0 until 12) {
          L1_wr_addr(i) := 0.U
          BNArray_ctrl(i) := 2.U
          Relu6Array_ctrl(i) := 1.U
        }
      }

      //PE state
      when((count >= 0.U) && (count <= 11.U)){
        PEArray_ctrl(2).control := 2.U
        PEArray_ctrl(2).mask := PEArray_ctrl(2).mask >> 1
      }

      //read
      when(count >= 1.U && count <= 391.U){
        L1_rd_addr(read_index) := L1_rd_addr(read_index) + 1
        when((count % 8.U === 7.U) && (count % 96.U =/= 95.U)){
          read_index := read_index + 1
          PE_rd_data_mux := PE_rd_data_mux + 1
        }
        when(count % 96.U === 95.U){
          read_index := 0.U
        }
      }

      //write
      when(count === 2.U){
        for (i <- 0 until 1) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 3.U){
        for (i <- 0 until 2) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 4.U){
        for (i <- 0 until 3) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 5.U){
        for (i <- 0 until 4) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 6.U){
        for (i <- 0 until 5) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 7.U){
        for (i <- 0 until 6) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 8.U){
        for (i <- 0 until 7) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 9.U){
        for (i <- 0 until 8) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 10.U){
        for (i <- 0 until 9) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 11.U){
        for (i <- 0 until 10) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 12.U){
        for (i <- 0 until 11) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when((count >= 13.U) && (count <= 393.U)){
        for (i <- 0 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 394.U){
        for (i <- 1 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 395.U){
        for (i <- 2 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 396.U){
        for (i <- 3 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 397.U){
        for (i <- 4 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 398.U){
        for (i <- 5 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 399.U){
        for (i <- 6 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 400.U){
        for (i <- 7 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 401.U){
        for (i <- 8 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 402.U){
        for (i <- 9 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 403.U){
        for (i <- 10 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      when(count === 404.U){
        for (i <- 11 until 12) {
          L1_wrEna(i) := true.B
          L1_wr_addr(i) := L1_wr_addr(i) + 1
        }
      }
      //next state
      when(count === 405.U){
        for (i <- 0 until 12) {
          L1_wrEna(i) := false.B
        }
        count := 0.U
        state := gru
      }


    }

    is(gru) {

    }

    is(fc) {

    }
  }
}


