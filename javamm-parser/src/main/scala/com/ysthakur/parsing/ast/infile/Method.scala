package com.ysthakur.parsing.ast.infile
import com.ysthakur.parsing.lexer.Modifier

import scala.collection.mutable.ListBuffer

case class Method(val modifiers: ListBuffer[Modifier]) extends TextNode with HasModifiers {
  def text: String = ???
}
