package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.Types._

import scala.collection.mutable.ListBuffer

case class ModifierList(modifiers: ListBuffer[Modifier]) extends Node {
  override def text: String = modifiers.mkString(" ")
}
