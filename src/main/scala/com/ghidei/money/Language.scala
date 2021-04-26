package com.ghidei.money
import com.ghidei.money.Language._

/**
 * An ISO 639 alpha-2 language code.
 * Follows: https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes
 */
sealed trait Language { self =>
  def code: String = self match {
    case Sv => Sv.Code
    case Fi => Fi.Code
    case No => No.Code
  }
}

object Language {
  case object Sv extends Language {
    val Code = "sv"
  }
  case object Fi extends Language {
    val Code = "fi"
  }
  case object No extends Language {
    val Code = "no"
  }
}
