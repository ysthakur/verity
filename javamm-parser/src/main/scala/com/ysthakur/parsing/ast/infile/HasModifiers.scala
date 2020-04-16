package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.lexer.Modifier

trait HasModifiers {
  def modifiers: Iterable[Modifier]
}
