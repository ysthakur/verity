package com.ysthakur.javamm.parsing.ast.infile

trait HasType {
  def myType: Option[ITypeRef]
  def myType_=(newType: ITypeRef): Unit
}
