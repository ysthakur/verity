package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.ast.Reference

trait ITypeRef extends Node {

}

case class TypeRef() extends Reference[TypeDef] {
  override def resolve: Option[TypeDef] = ???
}
