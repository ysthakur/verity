package verity.ast.infile

import verity.parsing._

trait Type extends Node

case class TypeRef(name: Name, args: Seq[Type]) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}

class TypeRange(var upperBound: Type, var lowerBound: Type) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}