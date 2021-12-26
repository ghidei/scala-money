package com.ghidei.money

import cats.Show

sealed trait CurrencyError extends Exception

object CurrencyError {

  implicit val s: Show[CurrencyError] = {
    case Mismatch(reason)          => s"Currency Mismatch error with reason: $reason"
    case ThrowableError(e, reason) => s"Currency Throwable Error with error: $e and reason: $reason"
  }

  case class Mismatch(reason: String) extends CurrencyError

  object Mismatch {
    def apply(fst: String, snd: String): Mismatch = new Mismatch(reason = s"Currency: $fst does not match: $snd.")
  }

  case class ThrowableError(e: Throwable, reason: String) extends CurrencyError

}
