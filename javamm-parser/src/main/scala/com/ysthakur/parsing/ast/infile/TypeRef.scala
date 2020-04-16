package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.ast.Reference

case class TypeRef() extends Reference[TypeDef] {
  override def resolve: TypeDef = ???
}
