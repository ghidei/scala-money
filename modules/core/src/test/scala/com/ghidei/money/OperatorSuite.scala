/*
 * MIT License
 * 
 * Copyright 2022 Yonas Ghidei
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package com.ghidei.money

import com.ghidei.money.Currency._
import munit._

import java.util.{Currency => JCurrency}
import scala.util.Try

class MoneyMinorSuite extends FunSuite {

  import MoneyMinor._
  val m1 = MoneyMinor.SEK(100)
  val m2 = MoneyMinor.SEK(200)
  val m3 = MoneyMinor.unsafeFromString(300, "SEK")
  val m4 = MoneyMinor.unsafeFromString(400, "EUR")
  val m5 = MoneyMinor.SEK(100)
  val m6 = MoneyMinor.unsafeFromString(300, "SEK")
  val m7 = MoneyMinor.unsafeFromString(400, "EUR")
  val m8 = MoneyMinor.unsafeFromString(400, "SEK")
  val m9 = MoneyMinor.SEK(-100)

  test("constructor") {
    assertEquals(MoneyMinor.EUR(300), new MoneyMinor(300, Currency.Eur()))
    assertEquals(MoneyMinor.SEK(300), new MoneyMinor(300, Currency.Sek()))
    assertEquals(MoneyMinor(300, Currency.Eur()), new MoneyMinor(300, Currency.Eur()))
    assertEquals(MoneyMinor(300, JCurrency.getInstance("EUR")), new MoneyMinor(300, Misc.unsafeFromString("EUR")))
    assertEquals(MoneyMinor.fromString(300, "EUR"), Try(new MoneyMinor(300, Misc.unsafeFromString("EUR"))))
    assertEquals(MoneyMinor.unsafeFromString(300, "EUR"), new MoneyMinor(300, Misc.unsafeFromString("EUR")))
  }

  test("+") {
    assertEquals(m1 + m2, MoneyMinor.SEK(300))
    assertEquals(m1 + m3, Right(MoneyMinor.SEK(400)))
    assertEquals(m3 + m1, Right(MoneyMinor.SEK(400)))
    assertEquals(m3 + m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("-") {
    assertEquals(m1 - m2, MoneyMinor.SEK(-100))
    assertEquals(m1 - m3, Right(MoneyMinor.SEK(-200)))
    assertEquals(m3 - m1, Right(MoneyMinor.SEK(200)))
    assertEquals(m3 - m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("*") {
    assertEquals(m1 * m2, MoneyMinor.SEK(20000))
    assertEquals(m1 * m3, Right(MoneyMinor.SEK(30000)))
    assertEquals(m3 * m1, Right(MoneyMinor.SEK(30000)))
    assertEquals(m3 * m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("/") {
    assertEquals(m1 / m2, MoneyMinor.SEK(0))
    assertEquals(m1 / m3, Right(MoneyMinor.SEK(0)))
    assertEquals(m3 / m1, Right(MoneyMinor.SEK(3)))
    assertEquals(m3 / m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("%") {
    assertEquals(m1 % m2, MoneyMinor.SEK(100))
    assertEquals(m1 % m3, Right(MoneyMinor.SEK(100)))
    assertEquals(m3 % m1, Right(MoneyMinor.SEK(0)))
    assertEquals(m3 % m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test(">") {
    assertEquals(m1 > m2, false)
    assertEquals(m2 > m1, true)
    assertEquals(m3 > m1, Right(true))
    assertEquals(m3 > m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("<") {
    assertEquals(m1 < m2, true)
    assertEquals(m2 < m1, false)
    assertEquals(m3 < m1, Right(false))
    assertEquals(m3 < m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test(">=") {
    assertEquals(m1 >= m2, false)
    assertEquals(m2 >= m1, true)
    assertEquals(m3 >= m1, Right(true))
    assertEquals(m3 >= m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("<=") {
    assertEquals(m1 <= m2, true)
    assertEquals(m2 <= m1, false)
    assertEquals(m3 <= m1, Right(false))
    assertEquals(m3 <= m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("===") {
    assertEquals(m1 === m5, true)
    assertEquals(m2 === m1, false)
    assertEquals(m3 === m6, Right(true))
    assertEquals(m4 === m7, Right(true))
    assertEquals(m6 === m7, Left(CurrencyError.Mismatch("SEK", "EUR")))
    assertEquals(m6 === m8, Right(false))
  }

  test("=!=") {
    assertEquals(m1 =!= m5, false)
    assertEquals(m2 =!= m1, true)
    assertEquals(m3 =!= m6, Right(false))
    assertEquals(m4 =!= m7, Right(false))
    assertEquals(m6 =!= m7, Left(CurrencyError.Mismatch("SEK", "EUR")))
    assertEquals(m6 =!= m8, Right(true))
  }

  test("max") {
    assertEquals(m1 max m2, MoneyMinor.SEK(200))
    assertEquals(m2 max m2, MoneyMinor.SEK(200))
    assertEquals(m1 max m3, Right(MoneyMinor.SEK(300)))
    assertEquals(m3 max m1, Right(MoneyMinor.SEK(300)))
    assertEquals(m3 max m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("min") {
    assertEquals(m1 min m2, MoneyMinor.SEK(100))
    assertEquals(m2 min m2, MoneyMinor.SEK(200))
    assertEquals(m1 min m3, Right(MoneyMinor.SEK(100)))
    assertEquals(m3 min m1, Right(MoneyMinor.SEK(100)))
    assertEquals(m3 min m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("abs") {
    assertEquals(m1.abs, MoneyMinor.SEK(100))
    assertEquals(m9.abs, MoneyMinor.SEK(100))
  }

  test("unary_+") {
    assertEquals(m1.unary_+, MoneyMinor.SEK(100))
    assertEquals(m9.unary_+, MoneyMinor.SEK(-100))
  }

  test("unary_-") {
    assertEquals(m1.unary_-, MoneyMinor.SEK(-100))
    assertEquals(m9.unary_-, MoneyMinor.SEK(100))
  }

}

class MoneyMajorSuite extends FunSuite {
  import MoneyMajor._
  val m1 = MoneyMajor.SEK(100)
  val m2 = MoneyMajor.SEK(200)
  val m3 = MoneyMajor.unsafeFromString(300, "SEK")
  val m4 = MoneyMajor.unsafeFromString(400, "EUR")
  val m5 = MoneyMajor.SEK(100)
  val m6 = MoneyMajor.unsafeFromString(300, "SEK")
  val m7 = MoneyMajor.unsafeFromString(400, "EUR")
  val m8 = MoneyMajor.unsafeFromString(400, "SEK")
  val m9 = MoneyMajor.SEK(-100)

  test("constructor") {
    assertEquals(MoneyMajor.EUR(300), new MoneyMajor(300, Currency.Eur()))
    assertEquals(MoneyMajor.SEK(300), new MoneyMajor(300, Currency.Sek()))
    assertEquals(MoneyMajor(300, Currency.Eur()), new MoneyMajor(300, Currency.Eur()))
    assertEquals(MoneyMajor(300, JCurrency.getInstance("EUR")), new MoneyMajor(300, Misc.unsafeFromString("EUR")))
    assertEquals(MoneyMajor.fromString(300, "EUR"), Try(new MoneyMajor(300, Misc.unsafeFromString("EUR"))))
    assertEquals(MoneyMajor.unsafeFromString(300, "EUR"), new MoneyMajor(300, Misc.unsafeFromString("EUR")))
  }

  test("+") {
    assertEquals(m1 + m2, MoneyMajor.SEK(300))
    assertEquals(m1 + m3, Right(MoneyMajor.SEK(400)))
    assertEquals(m3 + m1, Right(MoneyMajor.SEK(400)))
    assertEquals(m3 + m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("-") {
    assertEquals(m1 - m2, MoneyMajor.SEK(-100))
    assertEquals(m1 - m3, Right(MoneyMajor.SEK(-200)))
    assertEquals(m3 - m1, Right(MoneyMajor.SEK(200)))
    assertEquals(m3 - m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("*") {
    assertEquals(m1 * m2, MoneyMajor.SEK(20000))
    assertEquals(m1 * m3, Right(MoneyMajor.SEK(30000)))
    assertEquals(m3 * m1, Right(MoneyMajor.SEK(30000)))
    assertEquals(m3 * m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("/") {
    assertEquals(m1 / m2, MoneyMajor.SEK(0.5))
    assertEquals(m1 / m8, Right(MoneyMajor.SEK(0.25)))
    assertEquals(m3 / m1, Right(MoneyMajor.SEK(3)))
    assertEquals(m3 / m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("%") {
    assertEquals(m1 % m2, MoneyMajor.SEK(100))
    assertEquals(m1 % m3, Right(MoneyMajor.SEK(100)))
    assertEquals(m3 % m1, Right(MoneyMajor.SEK(0)))
    assertEquals(m3 % m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test(">") {
    assertEquals(m1 > m2, false)
    assertEquals(m2 > m1, true)
    assertEquals(m3 > m1, Right(true))
    assertEquals(m3 > m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("<") {
    assertEquals(m1 < m2, true)
    assertEquals(m2 < m1, false)
    assertEquals(m3 < m1, Right(false))
    assertEquals(m3 < m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test(">=") {
    assertEquals(m1 >= m2, false)
    assertEquals(m2 >= m1, true)
    assertEquals(m3 >= m1, Right(true))
    assertEquals(m3 >= m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("<=") {
    assertEquals(m1 <= m2, true)
    assertEquals(m2 <= m1, false)
    assertEquals(m3 <= m1, Right(false))
    assertEquals(m3 <= m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("===") {
    assertEquals(m1 === m5, true)
    assertEquals(m2 === m1, false)
    assertEquals(m3 === m6, Right(true))
    assertEquals(m4 === m7, Right(true))
    assertEquals(m6 === m7, Left(CurrencyError.Mismatch("SEK", "EUR")))
    assertEquals(m6 === m8, Right(false))
  }

  test("=!=") {
    assertEquals(m1 =!= m5, false)
    assertEquals(m2 =!= m1, true)
    assertEquals(m3 =!= m6, Right(false))
    assertEquals(m4 =!= m7, Right(false))
    assertEquals(m6 =!= m7, Left(CurrencyError.Mismatch("SEK", "EUR")))
    assertEquals(m6 =!= m8, Right(true))
  }

  test("max") {
    assertEquals(m1 max m2, MoneyMajor.SEK(200))
    assertEquals(m2 max m2, MoneyMajor.SEK(200))
    assertEquals(m1 max m3, Right(MoneyMajor.SEK(300)))
    assertEquals(m3 max m1, Right(MoneyMajor.SEK(300)))
    assertEquals(m3 max m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("min") {
    assertEquals(m1 min m2, MoneyMajor.SEK(100))
    assertEquals(m2 min m2, MoneyMajor.SEK(200))
    assertEquals(m1 min m3, Right(MoneyMajor.SEK(100)))
    assertEquals(m3 min m1, Right(MoneyMajor.SEK(100)))
    assertEquals(m3 min m4, Left(CurrencyError.Mismatch("SEK", "EUR")))
  }

  test("abs") {
    assertEquals(m1.abs, MoneyMajor.SEK(100))
    assertEquals(m9.abs, MoneyMajor.SEK(100))
  }

  test("unary_-") {
    assertEquals(m1.unary_-, MoneyMajor.SEK(-100))
    assertEquals(m9.unary_-, MoneyMajor.SEK(100))
  }

}
