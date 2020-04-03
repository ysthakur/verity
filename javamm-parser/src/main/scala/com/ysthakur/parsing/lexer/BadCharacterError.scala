package com.ysthakur.parsing.lexer

import com.ysthakur.CompilationError

case class BadCharacterError(char: Char, row: Int=0, col: Int=0, startOffset: Int=0)
    extends CompilationError(s"Bad character($char) at ($row,$col), offset=$startOffset")
