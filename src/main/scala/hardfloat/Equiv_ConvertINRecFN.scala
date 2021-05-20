
/*============================================================================

This Chisel source file is part of a pre-release version of the HardFloat IEEE
Floating-Point Arithmetic Package, by John R. Hauser (with some contributions
from Yunsup Lee and Andrew Waterman, mainly concerning testing).

Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016 The Regents of the
University of California.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions, and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the University nor the names of its contributors may
    be used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================*/

package hardfloat

import Chisel._
import chisel3.dontTouch

class
    Equiv_INToRecFN(intWidth: Int, expWidth: Int, sigWidth: Int)
    extends Module
{
    val io = new Bundle {
        val in = Bits(INPUT, intWidth)
        val roundingMode   = UInt(INPUT, 3)
        val detectTininess = UInt(INPUT, 1)
        val signedIn = UInt(INPUT, 1)

        val out = Bits(OUTPUT, expWidth + sigWidth)
        val exceptionFlags = Bits(OUTPUT, 5)
        val isGoodRecFN = Bits(OUTPUT, 1)
    }

    val iNToRecFN = Module(new INToRecFN(intWidth, expWidth, sigWidth))
    iNToRecFN.io.signedIn := io.signedIn
    iNToRecFN.io.in := io.in
    iNToRecFN.io.roundingMode   := io.roundingMode
    iNToRecFN.io.detectTininess := io.detectTininess

    io.out := fNFromRecFN(expWidth, sigWidth, iNToRecFN.io.out)
    io.exceptionFlags := iNToRecFN.io.exceptionFlags
    io.isGoodRecFN := isGoodRecFN(expWidth, sigWidth, io.out)
    dontTouch(io.isGoodRecFN)
}

class Equiv_I32ToRecF16 extends Equiv_INToRecFN(32, 5, 11)
class Equiv_I32ToRecF32 extends Equiv_INToRecFN(32, 8, 24)
class Equiv_I32ToRecF64 extends Equiv_INToRecFN(32, 11, 53)
class Equiv_I64ToRecF16 extends Equiv_INToRecFN(64, 5, 11)
class Equiv_I64ToRecF32 extends Equiv_INToRecFN(64, 8, 24)
class Equiv_I64ToRecF64 extends Equiv_INToRecFN(64, 11, 53)


class
    Equiv_RecFNToIN(expWidth: Int, sigWidth: Int, intWidth: Int)
    extends Module
{
    val io = new Bundle {
        val in = Bits(INPUT, expWidth + sigWidth + 1)
        val roundingMode = UInt(INPUT, 3)
        val signedOut = UInt(INPUT, 1)

        val out = Bits(OUTPUT, intWidth)
        val exceptionFlags = Bits(OUTPUT, 5)
        val isGoodRecFN = Bits(OUTPUT, 1)
    }

    val recFNToIN = Module(new RecFNToIN(expWidth, sigWidth, intWidth))
    recFNToIN.io.in := io.in
    recFNToIN.io.roundingMode := io.roundingMode
    recFNToIN.io.signedOut := io.signedOut

    io.out := recFNToIN.io.out
    io.exceptionFlags :=
        Cat(recFNToIN.io.intExceptionFlags(2, 1).orR,
            UInt(0, 3),
            recFNToIN.io.intExceptionFlags(0)
        )
    io.isGoodRecFN := isGoodRecFN(expWidth, sigWidth, io.in)
    dontTouch(io.isGoodRecFN)
}

class Equiv_RecF16ToI32 extends Equiv_RecFNToIN(5, 11, 32)
class Equiv_RecF16ToI64 extends Equiv_RecFNToIN(5, 11, 64)
class Equiv_RecF32ToI32 extends Equiv_RecFNToIN(8, 24, 32)
class Equiv_RecF32ToI64 extends Equiv_RecFNToIN(8, 24, 64)
class Equiv_RecF64ToI32 extends Equiv_RecFNToIN(11, 53, 32)
class Equiv_RecF64ToI64 extends Equiv_RecFNToIN(11, 53, 64)

