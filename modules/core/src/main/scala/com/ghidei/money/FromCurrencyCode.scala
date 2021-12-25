package com.ghidei.money

trait FromCurrencyCode[A] {
  def fromCode(currencyCode: String): Either[CurrencyError, Currency[A]]
}

object FromCurrencyCode {
  def apply[A](implicit ev: FromCurrencyCode[A]): FromCurrencyCode[A] = ev
}
