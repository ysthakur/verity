package verity.ast.infile

import verity.parsing.{TextRange, HasText}

import scala.collection.mutable.ListBuffer

case class TypeParam(name: Name, bounds: ListBuffer[TypeParamBound], override val textRange: TextRange) extends HasText, NamedTree, TypeDef {
  override def text: String = if (bounds.isEmpty) "name" else s"name "
}

case class TypeParamBound(boundType: BoundType, typeRepr: ITypeRef)

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}