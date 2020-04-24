package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.Reference

trait ITypeRef extends Node {

}

case class TypeRef() extends Reference[TypeDef] {
  override def resolve: Option[TypeDef] = ???
}
