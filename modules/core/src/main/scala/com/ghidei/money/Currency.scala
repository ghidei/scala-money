package com.ghidei.money

import cats.Show
import cats.implicits.catsSyntaxEither
import com.ghidei.money.Currency._

import java.util.{Currency => JavaCurrency}
import scala.util.Try

sealed trait Currency[A] { self =>

  def code: String =
    self match {
      case _: Currency.Eur     => Eur.Code
      case _: Currency.Sek     => Sek.Code
      case misc: Currency.Misc => misc.javaCurrency.getCurrencyCode.toUpperCase
    }

  def conversionFactor: Int =
    Math.pow(10, fractionalDigits.toDouble).toInt

  def fractionalDigits: Int =
    toJavaCurrency.getDefaultFractionDigits

  def narrow[B](implicit ev: A =:= B): B = self.asInstanceOf[B]

  def toJavaCurrency: JavaCurrency =
    self match {
      case _: Currency.Eur     => JavaCurrency.getInstance(Eur.Code)
      case _: Currency.Sek     => JavaCurrency.getInstance(Sek.Code)
      case misc: Currency.Misc => misc.javaCurrency
    }

}

object Currency {

  implicit def s[A]: Show[Currency[A]] = _.code

  def EUR: Currency[Eur] = Eur()

  case class Eur() extends Currency[Eur]

  object Eur {

    val Code = "EUR"

    implicit val c: FromCurrencyCode[Eur] = (currencyCode: String) =>
      Either.cond(currencyCode == Code, Eur(), CurrencyError.Mismatch(Code, currencyCode))

  }

  def SEK: Currency[Sek] = Sek()

  case class Sek() extends Currency[Sek]

  object Sek {

    val Code = "SEK"

    implicit val c: FromCurrencyCode[Sek] = (currencyCode: String) =>
      Either.cond(currencyCode == Code, Sek(), CurrencyError.Mismatch(Code, currencyCode))

  }

  def MISC(javaCurrency: JavaCurrency): Misc = Misc(javaCurrency)

  case class Misc(javaCurrency: JavaCurrency) extends Currency[Misc]

  object Misc {

    implicit val c: FromCurrencyCode[Misc] = (currencyCode: String) =>
      fromString(currencyCode).toEither
        .leftMap(CurrencyError.ThrowableError(_, s"Failed to create Java Currency from code: $currencyCode"))

    def unsafeFromString(currencyCode: String): Misc = new Misc(JavaCurrency.getInstance(currencyCode.toUpperCase))

    def fromString(currencyCode: String): Try[Misc] = Try(unsafeFromString(currencyCode))

  }

}
