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

package com.ghidei.money.circe

import com.ghidei.money._
import io.circe._

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
