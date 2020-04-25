package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

trait IMethodLike extends HasModifiers {
  def returnType: TypeRef
  def name: String
  def ctparams: CTParamList
  def params: ParamList
  
  def body: Option[Block|Expr]
  def body_=(newBody: Option[Block|Expr]): Unit
}
