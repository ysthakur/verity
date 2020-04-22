package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.Reference

case class TypeRef() extends Reference[TypeDef] {
  override def resolve: TypeDef = ???
}
