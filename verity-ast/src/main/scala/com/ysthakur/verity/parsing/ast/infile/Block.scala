package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.ast.infile.expr.Expr
import com.ysthakur.verity.parsing.TextRange

import scala.collection.mutable.ListBuffer

trait Block extends Node {
  def stmts: ListBuffer[Statement]
  override def text: String = s"{${stmts.map(_.text).mkString}}"
}

case class BlockExpr(override val stmts: ListBuffer[Statement], override val textRange: TextRange) extends Block with Expr {
  
}

case class BlockStmt(override val stmts: ListBuffer[Statement], override val textRange: TextRange) extends Block with Statement {
  
}