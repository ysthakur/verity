package com.ysthakur.parsing.lexer

  def toStr(chars: Iterable[Char]): String = chars match {
    case cs: CharSequence => chars.toString()
    case _                => chars.mkString
  }

  /*implicit*/ def toRegex(regex: String): RegexPattern = RegexPattern(regex)