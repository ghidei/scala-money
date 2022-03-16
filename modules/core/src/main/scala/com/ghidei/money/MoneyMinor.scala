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

import java.util.{Currency => JavaCurrency}
import scala.util.Try

/**
 * Follows: https://en.wikipedia.org/wiki/ISO_4217
 *
 * @param amount: The amount in MINOR units, e.g. Swedish "öre" or US "cents".
 * @param currency: The currency of the amount.
 *
 * The `+` operator will NOT be found on this data type unless explicitly imported by:
 * import com.ghidei.money.MoneyMinor._
 * due to the precedence that the `+` method defined in Scala Predef has.
 *
 * We express amounts in minor units according to the ISO 4217 standard.
 * This representation means that currencies are expressed in the SMALLEST unit.
 * Examples:
 *   USD with 1000 representing $$10,
 *   GBP with 500 representing £5,
 *   EUR with 50 representing €0.50
 *   SEK with 100 representing 1kr.
 */
case class MoneyMinor[A](amount: Long, currency: Currency[A]) { self =>

  def abs: MoneyMinor[A] =
    MoneyMinor(self.amount.abs, self.currency)

  def unary_+ : MoneyMinor[A] =
    MoneyMinor(self.amount.unary_+, currency)

  def unary_- : MoneyMinor[A] =
    MoneyMinor(self.amount.unary_-, currency)

}

object MoneyMinor {

  def EUR(minorUnit: Long): MoneyMinor[Currency.Eur] =
    new MoneyMinor(minorUnit, Currency.EUR)

  def SEK(minorUnit: Long): MoneyMinor[Currency.Sek] =
    new MoneyMinor(minorUnit, Currency.SEK)

  def apply[A](minorUnit: Long, currency: Currency[A]): MoneyMinor[A] =
    new MoneyMinor(minorUnit, currency)

  def apply(minorUnit: Long, currency: JavaCurrency): MoneyMinor[Currency.Misc] =
    new MoneyMinor(minorUnit, Currency.Misc(currency))

  def fromString(minorUnit: Long, currencyCode: String): Try[MoneyMinor[Currency.Misc]] =
    Currency.Misc.fromString(currencyCode).map(misc => new MoneyMinor(minorUnit, misc))

  def unsafeFromString(minorUnit: Long, currencyCode: String): MoneyMinor[Currency.Misc] =
    new MoneyMinor(minorUnit, Currency.Misc.unsafeFromString(currencyCode))

  implicit class MinorUnitMainSyntax[A](private val self: MoneyMinor[A]) {

    def +(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount + that.amount, self.currency)

    def +(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount + that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def -(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount - that.amount, self.currency)

    def -(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount - that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def *(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount * that.amount, self.currency)

    def *(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount * that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def /(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount / that.amount, self.currency)

    def /(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount / that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def %(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount % that.amount, self.currency)

    def %(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount % that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >(that: MoneyMinor[A]): Boolean =
      self.amount > that.amount

    def >(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount > that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <(that: MoneyMinor[A]): Boolean =
      self.amount < that.amount

    def <(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount < that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >=(that: MoneyMinor[A]): Boolean =
      self.amount >= that.amount

    def >=(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount >= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <=(that: MoneyMinor[A]): Boolean =
      self.amount <= that.amount

    def <=(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount <= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def ===(that: MoneyMinor[A]): Boolean =
      self.amount == that.amount

    def ===(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount == that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def =!=(that: MoneyMinor[A]): Boolean =
      self.amount != that.amount

    def =!=(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code != that.currency.code,
        self.amount == that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def max(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount.max(that.amount), self.currency)

    def max(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount.max(that.amount), self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def min(that: MoneyMinor[A]): MoneyMinor[A] =
      MoneyMinor(self.amount.min(that.amount), self.currency)

    def min(that: MoneyMinor[Currency.Misc]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount.min(that.amount), self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def readableFormat(country: Country, lang: Language): String =
      toMajor.readableFormat(country, lang)

    def toMajor: MoneyMajor[A] = {
      val majorAmount = BigDecimal(BigInt(self.amount), self.currency.fractionalDigits)
      MoneyMajor(majorAmount, self.currency)
    }

    def toMisc: MoneyMinor[Currency.Misc] =
      MoneyMinor(self.amount, self.currency.toJavaCurrency)

  }

  implicit class MinorUnitSekSyntax(private val self: MoneyMinor[Currency.Sek]) {

    def readableFormat: String =
      toMajor.readableFormat

    def toMajor: MoneyMajor[Currency.Sek] = {
      val majorAmount = BigDecimal(BigInt(self.amount), self.currency.fractionalDigits)
      MoneyMajor(majorAmount, self.currency)
    }

  }

  implicit class MinorUnitMiscSyntax[A](private val self: MoneyMinor[Currency.Misc]) {

    def +(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMinor(self.amount + that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def -(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMinor(self.amount - that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def *(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMinor(self.amount * that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def /(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMinor(self.amount / that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def %(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMinor(self.amount % that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def >(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount > that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount < that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >=(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount >= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <=(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount <= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def ===(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount == that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def =!=(that: MoneyMinor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount != that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def max(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount.max(that.amount), that.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def min(that: MoneyMinor[A]): Either[CurrencyError, MoneyMinor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMinor(self.amount.min(that.amount), that.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

  }

}
