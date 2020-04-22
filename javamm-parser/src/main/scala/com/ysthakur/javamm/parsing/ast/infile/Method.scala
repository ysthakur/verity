package com.ysthakur.javamm.parsing.ast.infile
import com.ysthakur.javamm.parsing.lexer.Modifier

import scala.collection.mutable.ListBuffer

case class Method(val modifiers: ModifierList, returnType: TypeRef) 
    extends Node with HasModifiers {
  def text: String = ???
}
