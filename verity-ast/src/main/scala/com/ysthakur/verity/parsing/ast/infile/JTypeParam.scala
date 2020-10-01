package com.ysthakur.verity.parsing.ast.infile
<<<<<<< HEAD:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/JTypeParam.scala

import com.ysthakur.verity.parsing.TextRange
=======
>>>>>>> master:javamm-ast/src/main/scala/com/ysthakur/javamm/parsing/ast/infile/JTypeParam.scala

import com.ysthakur.verity.parsing.TextRange

import scala.collection.mutable.ListBuffer

case class JTypeParam(name: String, bounds: ListBuffer[TypeParamBound], override val textRange: TextRange) extends CTParam {
  override def text: String = if (bounds.isEmpty) "name" else s"name "
}

case class TypeParamBound(boundType: BoundType, typeRepr: ITypeRef)

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}