package com.ghidei.money

import com.ghidei.money.{Country, Language}
import munit._

import scala.math.BigDecimal.RoundingMode

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

}
