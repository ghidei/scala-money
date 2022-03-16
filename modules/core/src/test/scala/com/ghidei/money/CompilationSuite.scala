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

import Currency._
import MoneyMajor._

class CompliationSuite extends FunSuite {

  val majorAmountSek: MoneyMajor[Sek] = MoneyMajor.SEK(100)
  val minorAmountSek: MoneyMinor[Sek] = MoneyMinor.SEK(100)
  val majorAmountEur: MoneyMajor[Eur] = MoneyMajor.EUR(100)
  val minorAmountEur: MoneyMinor[Eur] = MoneyMinor.EUR(100)

  test("Should not be able to use operators on different currencies [*]") {
    assertNoDiff(
      compileErrors("""majorAmountSek * majorAmountEur"""),
      """|error:
         |overloaded method * with alternatives:
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Misc])Either[com.ghidei.money.CurrencyError,com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]] <and>
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek])com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]
         | cannot be applied to (com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Eur])
         |majorAmountSek * majorAmountEur
         |               ^
         |""".stripMargin
    )
  }

  test("Should not be able to use operators on different currencies [+]") {
    assertNoDiff(
      compileErrors("""majorAmountSek + majorAmountEur"""),
      """|error:
         |overloaded method + with alternatives:
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Misc])Either[com.ghidei.money.CurrencyError,com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]] <and>
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek])com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]
         | cannot be applied to (com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Eur])
         |majorAmountSek + majorAmountEur
         |               ^
         |""".stripMargin
    )
  }

  test("Should not be able to use operators on different units") {
    assertNoDiff(
      compileErrors("""majorAmountSek * minorAmountSek"""),
      """|error:
         |overloaded method * with alternatives:
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Misc])Either[com.ghidei.money.CurrencyError,com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]] <and>
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek])com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]
         | cannot be applied to (com.ghidei.money.MoneyMinor[com.ghidei.money.Currency.Sek])
         |majorAmountSek * minorAmountSek
         |               ^
         |""".stripMargin
    )
  }

  test("Should not be able to use operators on different units and currencies") {
    assertNoDiff(
      compileErrors("""majorAmountSek + minorAmountEur"""),
      """|error:
         |overloaded method + with alternatives:
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Misc])Either[com.ghidei.money.CurrencyError,com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]] <and>
         |  (that: com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek])com.ghidei.money.MoneyMajor[com.ghidei.money.Currency.Sek]
         | cannot be applied to (com.ghidei.money.MoneyMinor[com.ghidei.money.Currency.Eur])
         |majorAmountSek + minorAmountEur
         |               ^
         |""".stripMargin
    )
  }

}
