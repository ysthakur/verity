package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.Reference
import com.ysthakur.javamm.parsing.ast.infile.TypeDef

trait ITypeRef extends Node {

}

case class TypeRef() extends Reference[TypeDef] with Node {
  override def resolve: Option[TypeDef] = ???
}
