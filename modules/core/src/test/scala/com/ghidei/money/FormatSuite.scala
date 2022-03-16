package com.ghidei.money

import com.ghidei.money.{Country, Language}
import munit._

class FormatSuite extends FunSuite {

  test("Readable format for money major SEK") {
    val sekMajor = MoneyMajor.SEK(450)
    assertEqNormalized(sekMajor.readableFormat, "450,00 kr")
  }

  test("Readable format for money major EUR") {
    val eurMajor = MoneyMajor.EUR(450)
    assertEqNormalized(eurMajor.readableFormat(Country.Fi, Language.Fi), "450,00 €")
  }

  test("Readable format for money major MISC") {
    val miscMajor = MoneyMajor.unsafeFromString(450, "EUR")
    assertEqNormalized(miscMajor.readableFormat(Country.Fi, Language.Fi), "450,00 €")
  }

  test("Readable format for money major MISC with lower case currency") {
    val miscMajor = MoneyMajor.unsafeFromString(450, "eur")
    assertEqNormalized(miscMajor.readableFormat(Country.Fi, Language.Fi), "450,00 €")
  }

  test("Readable format for money minor MISC with lower case currency") {
    val miscMinor = MoneyMinor.unsafeFromString(45000, "eur")
    assertEqNormalized(miscMinor.readableFormat(Country.Fi, Language.Fi), "450,00 €")
  }

  test("Readable format for money minor SEK no rounding needed") {
    val minorSek = MoneyMinor.SEK(200)
    assertEqNormalized(minorSek.readableFormat, "2,00 kr")
  }

  test("Readable format for money minor EUR no rounding needed") {
    val eurMinor = MoneyMinor.EUR(200)
    assertEqNormalized(eurMinor.readableFormat(Country.Fi, Language.Fi), "2,00 €")
  }

  test("Readable format for money minor MISC no rounding needed") {
    val miscMinor = MoneyMinor.unsafeFromString(200, "EUR")
    assertEqNormalized(miscMinor.readableFormat(Country.Fi, Language.Fi), "2,00 €")
  }

  test("Readable format for money minor SEK when rounded") {
    val sekMinor = MoneyMinor.SEK(199)
    assertEqNormalized(normalizeSpace(sekMinor.readableFormat), "1,99 kr")
  }

  test("Readable format for money minor MISC NOK in Norway") {
    val miscMajor = MoneyMajor.unsafeFromString(450, "NOK")
    assertEqNormalized(miscMajor.readableFormat(Country.No, Language.No), "kr 450,00")
  }

  def assertEqNormalized(actual: String, expected: String): Unit = {
    assertEquals(normalizeSpace(actual), normalizeSpace(expected))
  }

  // Depending on the JDK version, the formatter will use different space (ASCII 32 or 160)
  // between the amount and the currency symbol...
  def normalizeSpace(s: String): String = s.replace(32.toChar, 160.toChar)

}
