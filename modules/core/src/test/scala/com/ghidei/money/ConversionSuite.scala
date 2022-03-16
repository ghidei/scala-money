/*
 * Copyright (c) 2022 ghidei
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.ghidei.money

import munit._

class ConversionSuite extends FunSuite {

  test("major toReadable format for  minor") {
    val majorSek  = MoneyMajor.SEK(540.355)
    val majorEur  = MoneyMajor.EUR(500)
    val majorMisc = MoneyMajor.unsafeFromString(500, "EUR")

    intercept[ArithmeticException] {
      majorSek.unsafeToMinor
    }
    assert(majorSek.toMinor.isLeft, "Major SEK with fractional digits > 2 should fail")
    assert(majorEur.toMinor.isRight, "Major EUR with fractional digits < should succeed")
    assertEquals(majorEur.unsafeToMinor, MoneyMinor.EUR(50000))
    assertEquals(majorMisc.unsafeToMinor, MoneyMinor.unsafeFromString(50000, "EUR"))
  }

  test("minor toReadable format for  major using rounding mode") {
    val minorSek  = MoneyMinor.SEK(540)
    val minorEur  = MoneyMinor.EUR(590)
    val minorMisc = MoneyMinor.unsafeFromString(590, "EUR")
    assertEquals(minorSek.toMajor, MoneyMajor.SEK(5.40))
    assertEquals(minorEur.toMajor, MoneyMajor.EUR(5.90))
    assertEquals(minorMisc.toMajor, MoneyMajor.unsafeFromString(5.90, "EUR"))
  }

  test("minor toReadable format for  major with default half_up") {
    val minorSek  = MoneyMinor.SEK(540)
    val minorEur  = MoneyMinor.EUR(590)
    val minorMisc = MoneyMinor.unsafeFromString(590, "EUR")
    assertEquals(minorSek.toMajor, MoneyMajor.SEK(5.40))
    assertEquals(minorEur.toMajor, MoneyMajor.EUR(5.90))
    assertEquals(minorMisc.toMajor, MoneyMajor.unsafeFromString(5.90, "EUR"))
  }

  test("MoneyMajor#toMisc") {
    val majorSek = MoneyMajor.SEK(540)
    assertEquals(majorSek.toMisc, MoneyMajor.unsafeFromString(540, "SEK"))
  }

  test("MoneyMinor#toMisc") {
    val minorSek = MoneyMinor.SEK(540)
    assertEquals(minorSek.toMisc, MoneyMinor.unsafeFromString(540, "SEK"))
  }

}
