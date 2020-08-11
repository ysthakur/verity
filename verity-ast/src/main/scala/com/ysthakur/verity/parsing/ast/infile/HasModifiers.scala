package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.Node

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Node {
  def modifiers: ModifierList
  def modifiersText: String = modifiers.modifiers.mkString(" ")
}

//TODO decide if this should be replaced with a plain ListBuffer[Modifier]
case class ModifierList(modifiers: ListBuffer[Modifier], override val textRange: TextRange) extends Node {
  override def text: String = modifiers.mkString(" ")
}

enum Modifier(override val textRange: TextRange) extends Node {
  case PUBLIC(tr: TextRange) extends Modifier(tr)
  case PRIVATE(tr: TextRange) extends Modifier(tr)
  case PROTECTED(tr: TextRange) extends Modifier(tr)
  case DEFAULT(tr: TextRange) extends Modifier(tr)
  case STATIC(tr: TextRange) extends Modifier(tr)
  case ABSTRACT(tr: TextRange) extends Modifier(tr)
  case FINAL(tr: TextRange) extends Modifier(tr)
  case NATIVE(tr: TextRange) extends Modifier(tr)
  case TRANSIENT(tr: TextRange) extends Modifier(tr)
  case VOLATILE(tr: TextRange) extends Modifier(tr)
  case SYNCHRONIZED(tr: TextRange) extends Modifier(tr)
  case CONST(tr: TextRange) extends Modifier(tr)

  override def text: String = this.toString.toLowerCase
}