package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer._

case class Operator(symbol: SymbolTokenType) extends Node {
  def isBinary: Boolean = ???
}