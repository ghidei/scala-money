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
