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

import com.ghidei.money.circe.implicits._
import io.circe.parser._
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, Error}
import munit._

class JsonSuite extends FunSuite {

  val minorUnitSekString = """{"amount":500,"currency":"SEK","unit":"MINOR"}""".stripMargin
  val minorUnitEurString = """{"amount":500,"currency":"EUR","unit":"MINOR"}""".stripMargin
  val majorUnitSekString = """{"amount":500,"currency":"SEK","unit":"MAJOR"}""".stripMargin
  val majorUnitEurString = """{"amount":500,"currency":"EUR","unit":"MAJOR"}""".stripMargin

  test("Encoding MoneyMinor in SEK") {
    val expected = parse(minorUnitSekString)
    val actual   = MoneyMinor.SEK(500)
    assertEquals(expected, Right(actual.asJson))
  }

  test("Encoding MoneyMinor in EUR") {
    val expected = parse(minorUnitEurString)
    val actual   = MoneyMinor.EUR(500)
    assertEquals(expected, Right(actual.asJson))
  }

  test("Encoding MoneyMinor in MISC") {
    val expected = parse(minorUnitSekString)
    val actual   = MoneyMinor.unsafeFromString(500, "SEK")
    assertEquals(expected, Right(actual.asJson))
  }

  test("Encoding MoneyMajor in SEK") {
    val expected = parse(majorUnitSekString)
    val actual   = MoneyMajor.SEK(500)
    assertEquals(expected, Right(actual.asJson))
  }

  test("Encoding MoneyMajor in EUR") {
    val expected = parse(majorUnitEurString)
    val actual   = MoneyMajor.EUR(500)
    assertEquals(expected, Right(actual.asJson))
  }

  test("Encoding MoneyMajor in MISC") {
    val expected = parse(majorUnitSekString)
    val actual   = MoneyMajor.unsafeFromString(500, "SEK")
    assertEquals(expected, Right(actual.asJson))
  }

  test("Decoding MoneyMinor in SEK") {
    val expected: Either[Error, MoneyMinor[Currency.Sek]] = Right(MoneyMinor.SEK(500))
    val actual                                            = decode[MoneyMinor[Currency.Sek]](minorUnitSekString)
    assertEquals(expected, actual)
  }

  test("Decoding MoneyMinor in EUR") {
    val expected: Either[Error, MoneyMinor[Currency.Eur]] = Right(MoneyMinor.EUR(500))
    val actual                                            = decode[MoneyMinor[Currency.Eur]](minorUnitEurString)
    assertEquals(expected, actual)
  }

  test("Decoding MoneyMinor in MISC") {
    val expected: Either[Error, MoneyMinor[Currency.Misc]] = Right(MoneyMinor.unsafeFromString(500, "SEK"))
    val actual                                             = decode[MoneyMinor[Currency.Misc]](minorUnitSekString)
    assertEquals(expected, actual)
  }

  test("Decoding MoneyMajor in SEK") {
    val expected: Either[Error, MoneyMajor[Currency.Sek]] = Right(MoneyMajor.SEK(500))
    val actual                                            = decode[MoneyMajor[Currency.Sek]](majorUnitSekString)
    assertEquals(expected, actual)
  }

  test("Decoding MoneyMajor in EUR") {
    val expected: Either[Error, MoneyMajor[Currency.Eur]] = Right(MoneyMajor.EUR(500))
    val actual                                            = decode[MoneyMajor[Currency.Eur]](majorUnitEurString)
    assertEquals(expected, actual)
  }

  test("Decoding MoneyMajor in MISC") {
    val expected: Either[Error, MoneyMajor[Currency.Misc]] = Right(MoneyMajor.unsafeFromString(500, "SEK"))
    val actual                                             = decode[MoneyMajor[Currency.Misc]](majorUnitSekString)
    assertEquals(expected, actual)
  }

  test("Trying to decode to the wrong type: Major -> Minor") {
    val expected: Either[Error, MoneyMinor[Currency.Sek]] =
      Left(DecodingFailure("Expected unit: MINOR, but got: MAJOR.", Nil))
    val actual = decode[MoneyMinor[Currency.Sek]](majorUnitSekString)
    assertEquals(expected, actual)
  }

  test("Trying to decode to the wrong type: Minor -> Major") {
    val expected: Either[Error, MoneyMajor[Currency.Sek]] =
      Left(DecodingFailure("Expected unit: MAJOR, but got: MINOR.", Nil))
    val actual = decode[MoneyMajor[Currency.Sek]](minorUnitSekString)
    assertEquals(expected, actual)
  }

}
