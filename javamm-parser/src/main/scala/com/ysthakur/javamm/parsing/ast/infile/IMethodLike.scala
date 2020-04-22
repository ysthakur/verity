package com.ysthakur.javamm.parsing.ast.infile

trait IMethodLike extends HasModifiers {
  def returnType: TypeRef
  def name: String
  def params: ParamList
  def body: Option[Block|Statement]
}
