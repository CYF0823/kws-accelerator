package acceleration

import chisel3._
import chisel3.util._

import hardfloat._

//use Berkeley hardfloat library to bulid a standard floating point (IEEE 754) MAC:

class MulAddFN(expWidth: Int, sigWidth: Int) extends Module
{
    val io = IO( new Bundle {
  val op = Input(Bits(2.W))
        val a = Input(Bits((expWidth + sigWidth).W))
        val b = Input(Bits((expWidth + sigWidth).W))
        val c = Input(Bits((expWidth + sigWidth).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
  /*
        val expected = new Bundle {
            val out = Bits(INPUT, expWidth + sigWidth)
            val exceptionFlags = Bits(INPUT, 5)
            val recOut = Bits(OUTPUT, expWidth + sigWidth + 1)
        } 
  */
  
  val out = Output(Bits((expWidth + sigWidth).W))
        val exceptionFlags = Output(Bits(5.W))
    })

    val mulAddRecFN = Module(new MulAddRecFN(expWidth, sigWidth))
    mulAddRecFN.io.op := io.op
  //convert to redFN
    mulAddRecFN.io.a := recFNFromFN(expWidth, sigWidth, io.a)
    mulAddRecFN.io.b := recFNFromFN(expWidth, sigWidth, io.b)
    mulAddRecFN.io.c := recFNFromFN(expWidth, sigWidth, io.c)
    mulAddRecFN.io.roundingMode   := io.roundingMode
    mulAddRecFN.io.detectTininess := io.detectTininess

/*
    io.expected.recOut := recFNFromFN(expWidth, sigWidth, io.expected.out)
*/

  
  //convert to standard IEEE floating point
    io.out := fNFromRecFN(expWidth, sigWidth, mulAddRecFN.io.out)
    io.exceptionFlags := mulAddRecFN.io.exceptionFlags


}


//half precision floating point (16-bit / 1 sign + 5 exp + 10 significand)
class FP16MulAdder extends MulAddFN(5, 11)

//single precision floating point (32-bit / 1 sign + 8 exp + 23 significand)
class FP32MulAdder extends MulAddFN(8, 24)

//single precision floating point (64-bit / 1 sign + 11 exp + 52 significand)
class FP64MulAdder extends MulAddFN(11, 53)


// Google TPU :
// BFloat data format (16-bit / 1 sign + 8 exp + 7 significand)
class BF16MulAdder extends MulAddFN(8, 8)



///////////////////////////////////////// 