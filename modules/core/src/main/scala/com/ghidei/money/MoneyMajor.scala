package com.ghidei.money

import cats.implicits.catsSyntaxEither
import com.ghidei.money.Currency._
import com.ghidei.money.CurrencyError._

import java.text.NumberFormat
import java.util.{Currency => JavaCurrency, Locale}
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.util.Try

/**
 * Follows: https://en.wikipedia.org/wiki/ISO_4217
 *
 * @param amount: The amount in MAJOR units, e.g. Swedish "kr" or US "$".
 * @param currency: The currency of the amount.
 *
 * The `+` operator will NOT be found on this data type unless explicitly imported by:
 * import com.ghidei.money.MoneyMajor._
 * due to the precedence that the `+` method defined in Scala Predef has.
 *
 * We express amounts in major units according to the ISO 4217 standard.
 * This representation means that currencies are expressed in the BIGGEST unit.
 * Examples:
 *   USD with 10,03 representing $10 and 3 US CENTS,
 *   GBP with 5 representing £5,
 *   EUR with 5 representing €5
 *   SEK with 1,50 representing 1kr and 50 ÖRE.
 */
case class MoneyMajor[A](amount: BigDecimal, currency: Currency[A]) { self =>

  def abs: MoneyMajor[A] =
    MoneyMajor(self.amount.abs, self.currency)

  def unary_- : MoneyMajor[A] =
    MoneyMajor(self.amount.unary_-, currency)

}

object MoneyMajor {

  def EUR(majorUnit: BigDecimal): MoneyMajor[Currency.Eur] =
    new MoneyMajor(majorUnit, Currency.EUR)

  def SEK(majorUnit: BigDecimal): MoneyMajor[Currency.Sek] =
    new MoneyMajor(majorUnit, Currency.SEK)

  def apply[A](majorUnit: BigDecimal, currency: Currency[A]): MoneyMajor[A] =
    new MoneyMajor(majorUnit, currency)

  def apply(majorUnit: BigDecimal, currency: JavaCurrency): MoneyMajor[Currency.Misc] =
    new MoneyMajor(majorUnit, Currency.Misc(currency))

  def fromString(majorUnit: BigDecimal, currencyCode: String): Try[MoneyMajor[Currency.Misc]] =
    Currency.Misc.fromString(currencyCode).map(misc => new MoneyMajor(majorUnit, misc))

  def unsafeFromString(majorUnit: BigDecimal, currencyCode: String): MoneyMajor[Currency.Misc] =
    new MoneyMajor(majorUnit, Currency.Misc.unsafeFromString(currencyCode))

  implicit class MajorUnitMainSyntax[A](private val self: MoneyMajor[A]) {

    def +(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount + that.amount, self.currency)

    def +(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount + that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def -(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount - that.amount, self.currency)

    def -(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount - that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def *(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount * that.amount, self.currency)

    def *(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount * that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def /(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount / that.amount, self.currency)

    def /(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount / that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def %(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount % that.amount, self.currency)

    def %(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount % that.amount, self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >(that: MoneyMajor[A]): Boolean =
      self.amount > that.amount

    def >(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount > that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <(that: MoneyMajor[A]): Boolean =
      self.amount < that.amount

    def <(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount < that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >=(that: MoneyMajor[A]): Boolean =
      self.amount >= that.amount

    def >=(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount >= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <=(that: MoneyMajor[A]): Boolean =
      self.amount <= that.amount

    def <=(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount <= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def ===(that: MoneyMajor[A]): Boolean =
      self.amount == that.amount

    def ===(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount == that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def =!=(that: MoneyMajor[A]): Boolean =
      self.amount != that.amount

    def =!=(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount != that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def max(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount.max(that.amount), self.currency)

    def max(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount.max(that.amount), self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def min(that: MoneyMajor[A]): MoneyMajor[A] =
      MoneyMajor(self.amount.min(that.amount), self.currency)

    def min(that: MoneyMajor[Currency.Misc]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount.min(that.amount), self.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def toMinor: Either[CurrencyError.ThrowableError, MoneyMinor[A]] = {
      val frac = self.currency.fractionalDigits
      Try {
        val minorAmount = BigDecimal(self.amount.underlying.movePointRight(frac)).toLongExact
        MoneyMinor(minorAmount, self.currency)
      }.toEither
        .leftMap(e =>
          ThrowableError(e, s"$self likely contains more fractions than the currency's fractional digit: $frac")
        )
    }

    def toMisc: MoneyMajor[Misc] =
      MoneyMajor(self.amount, self.currency.toJavaCurrency)

    def readableFormat(country: Country, lang: Language): String = {
      val locale       = new Locale(lang.code, country.code)
      val formatter    = NumberFormat.getCurrencyInstance(locale)
      val javaCurrency = self.currency.toJavaCurrency
      formatter.setCurrency(javaCurrency)
      formatter.format(self.amount)
    }

    /**
     * WARNING: This function can throw a `java.lang.ArithmeticException`.
     *
     * This conversion will assume that the amount has max x precision, where
     * x represents the specific currency's fractional digits.
     * For SEK, USD and GBP (and most currencies) this number is 2.
     *
     * Example:
     * ERROR: 5.999 (MoneyMajor) throws an `ArithmeticException`
     * OK: 5.99 (MoneyMajor) => 599 (MoneyMinor)
     */
    def unsafeToMinor: MoneyMinor[A] = {
      val minorAmount = BigDecimal(self.amount.underlying.movePointRight(self.currency.fractionalDigits)).toLongExact
      MoneyMinor(minorAmount, self.currency)
    }

  }

  implicit class MajorUnitSekSyntax(private val self: MoneyMajor[Currency.Sek]) {

    def readableFormat: String = {
      val locale       = new Locale(Language.Sv.code, Country.Se.code)
      val formatter    = NumberFormat.getCurrencyInstance(locale)
      val javaCurrency = JavaCurrency.getInstance(self.currency.code)
      formatter.setCurrency(javaCurrency)
      formatter.format(self.amount)
    }

  }

  implicit class MajorUnitMiscSyntax[A](private val self: MoneyMajor[Currency.Misc]) {

    def refine[A](to: Currency[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == to.code,
      MoneyMajor(self.amount, to),
      CurrencyError.Mismatch(self.currency.code, to.code)
    )

    def +(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMajor(self.amount + that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def -(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMajor(self.amount - that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def *(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMajor(self.amount * that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def /(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMajor(self.amount / that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def %(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] = Either.cond(
      self.currency.code == that.currency.code,
      MoneyMajor(self.amount % that.amount, that.currency),
      CurrencyError.Mismatch(self.currency.code, that.currency.code)
    )

    def >(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount > that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount < that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def >=(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount >= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def <=(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount <= that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def ===(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount == that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def =!=(that: MoneyMajor[A]): Either[CurrencyError, Boolean] =
      Either.cond(
        self.currency.code == that.currency.code,
        self.amount != that.amount,
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def max(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount.max(that.amount), that.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

    def min(that: MoneyMajor[A]): Either[CurrencyError, MoneyMajor[A]] =
      Either.cond(
        self.currency.code == that.currency.code,
        MoneyMajor(self.amount.min(that.amount), that.currency),
        CurrencyError.Mismatch(self.currency.code, that.currency.code)
      )

  }

}
