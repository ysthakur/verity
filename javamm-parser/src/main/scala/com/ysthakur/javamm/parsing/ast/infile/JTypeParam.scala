package com.ysthakur.javamm.parsing.ast.infile

import scala.collection.mutable.ListBuffer

case class JTypeParam(name: String, bounds: ListBuffer[TypeParamBound]) extends CTParam {
  override def text: String = if (bounds.isEmpty) "name" else s"name "
}

case class TypeParamBound(boundType: BoundType, typeRepr: ITypeRef)

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}