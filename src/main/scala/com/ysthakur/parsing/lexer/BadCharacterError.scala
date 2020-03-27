package com.ysthakur.parsing.lexer

import com.ysthakur.CompilationError

case class BadCharacterError(char: Char, position: String) extends CompilationError(s"Bad character($char) at position ${position}")
