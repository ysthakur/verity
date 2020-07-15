package com.ysthakur.verity.parsing.lexer

import com.ysthakur.verity.CompilationError

case class BadCharacterError(char: Char, row: Int=0, col: Int=0, startOffset: Int=0)
    extends CompilationError(s"Bad character($char) at ($row,$col), offset=$startOffset")
