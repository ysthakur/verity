package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

case class TypeParam(
    name: String,
    bounds: ListBuffer[TypeParamBound],
    override val textRange: TextRange
) extends HasText, NamedTree, TypeDef {
  //TODO write this
  override def text: String = if (bounds.isEmpty) name else s"$name, ${bounds.view.map(_.text).mkString(",")}"
}

case class TypeParamBound(boundType: BoundType, typeRepr: TypeRef, textRange: TextRange) extends HasText {
  //TODO write this
  def text = ""
}

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}
