package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

import scala.collection.mutable.ListBuffer

trait Block extends Node {
  def stmts: ListBuffer[Statement]
  override def text: String = s"{${stmts.map(_.text).mkString}}"
}

case class BlockExpr(override val stmts: ListBuffer[Statement]) extends Block with Expr {
  
}

case class BlockStmt(override val stmts: ListBuffer[Statement]) extends Block with Statement {
  
}