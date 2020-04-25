package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ModifierTokenType

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Node {
  def modifiers: ModifierList
  def modifiersText: String = modifiers.modifiers.mkString(" ")
}

//TODO decide if this should be replaced with a plain ListBuffer[Modifier]
case class ModifierList(modifiers: ListBuffer[Modifier]) extends Node {
  override def text: String = modifiers.mkString(" ")
}

enum Modifier extends Node {
  case PUBLIC, PRIVATE,  PROTECTED,  DEFAULT,
  STATIC, ABSTRACT, FINAL, NATIVE, TRANSIENT,
  VOLATILE, SYNCHRONIZED, CONST

  override def text: String = this.toString.toLowerCase
}

object Modifier {
  def get(modTokenType: ModifierTokenType): Modifier = Modifier.valueOf(modTokenType.text)
}