package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.lexer.Modifier

trait HasModifiers {
  def modifiers: ModifierList
}
