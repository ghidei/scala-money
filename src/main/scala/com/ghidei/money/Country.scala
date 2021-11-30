package com.ghidei.money

import com.ghidei.money.Country._

/**
 * An ISO 3166 alpha-2 country code or a UN M.49 numeric-3 area code.
 * Follows: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
 */
sealed trait Country { self =>
  def code: String = self match {
    case Se => "SE"
    case Fi => "FI"
    case No => "NO"
  }
}

object Country {
  case object Se extends Country
  case object Fi extends Country
  case object No extends Country
}
