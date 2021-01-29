package verity.parsing.ast.infile

import verity.parsing.TextRange

import scala.collection.mutable.ListBuffer

case class JTypeParam(name: String, bounds: ListBuffer[TypeParamBound], override val textRange: TextRange) extends CTParam {
  override def text: String = if (bounds.isEmpty) "name" else s"name "
}

case class TypeParamBound(boundType: BoundType, typeRepr: ITypeRef)

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}