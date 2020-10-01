package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.TextRange

case class Rule(params: ParamList, override val textRange: TextRange) extends Node {
  override def text: String = s"rule${params.text}" //TODO implement this properly
}
