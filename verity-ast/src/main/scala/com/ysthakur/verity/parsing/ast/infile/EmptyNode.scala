package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.{TextRange, Position}

case class EmptyNode(start: Position) extends Node {
  override def text: String = ""
  override def textRange = TextRange(start, start)
}