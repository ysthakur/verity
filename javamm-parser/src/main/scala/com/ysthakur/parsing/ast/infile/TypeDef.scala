package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.lexer.Modifier

import scala.collection.mutable.ListBuffer

case class TypeDef(modifiers: List[Modifier]) extends TextNode with ParentNode {
  override type Child = Field | Method
  override val children: Iterable[Child] = ListBuffer()
  override def text: String = ??? //modifiers.map(_.text).mkString(" ")
}

enum TypeDefType {
  case CLASS, ABSTRACT_CLASS, INTERFACE, ENUM
}