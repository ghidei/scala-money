package com.ghidei.money.circe

import com.ghidei.money.Currency._
import com.ghidei.money.CurrencyError._
import com.ghidei.money.MoneyMajor._
import com.ghidei.money.MoneyMinor._
import com.ghidei.money.{Currency, FromCurrencyCode, MoneyMajor, MoneyMinor}
import io.circe._

import java.text.NumberFormat
import java.util.{Currency => JavaCurrency, Locale}
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.util.Try

package object implicits {

  implicit def currencyDecoder[A: FromCurrencyCode]: Decoder[Currency[A]] =
    Decoder[String].emap(s => FromCurrencyCode[A].fromCode(s).left.map(_.getMessage))

  implicit def currencyEncoder[A]: Encoder[Currency[A]] =
    Encoder[String].contramap(_.code)

  val CurrencyField = "currency"
  val AmountField   = "amount"
  val UnitField     = "unit"

  val MajorUnitValue = "MAJOR"

  implicit def encodeMajor[A]: Encoder[MoneyMajor[A]] = { majorAmount =>
    Json.obj(
      CurrencyField -> Encoder[Currency[A]].apply(majorAmount.currency),
      AmountField   -> Json.fromBigDecimal(majorAmount.amount),
      UnitField     -> Json.fromString(MajorUnitValue)
    )
  }

  implicit def decodeMajor[A: FromCurrencyCode]: Decoder[MoneyMajor[A]] = { hCursor =>
    for {
      amount   <- hCursor.downField(AmountField).as[BigDecimal]
      currency <- hCursor.downField(CurrencyField).as[Currency[A]]
      unit     <- hCursor.downField(UnitField).as[String]
      _ <- Either.cond(
             unit == MajorUnitValue,
             (),
             DecodingFailure(s"Expected unit: $MajorUnitValue, but got: $unit.", Nil)
           )
    } yield MoneyMajor(amount, currency)
  }

  val MinorUnitValue = "MINOR"

  implicit def encodeMinor[A]: Encoder[MoneyMinor[A]] = { minorUnit =>
    Json.obj(
      CurrencyField -> Encoder[Currency[A]].apply(minorUnit.currency),
      AmountField   -> Json.fromLong(minorUnit.amount),
      UnitField     -> Json.fromString(MinorUnitValue)
    )
  }

  implicit def decodeMinor[A: FromCurrencyCode]: Decoder[MoneyMinor[A]] = { hCursor =>
    for {
      amount   <- hCursor.downField(AmountField).as[Long]
      currency <- hCursor.downField(CurrencyField).as[Currency[A]]
      unit     <- hCursor.downField(UnitField).as[String]
      _ <- Either.cond(
             unit == MinorUnitValue,
             (),
             DecodingFailure(s"Expected unit: $MinorUnitValue, but got: $unit.", Nil)
           )
    } yield MoneyMinor(amount, currency)
  }

}
