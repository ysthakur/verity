package verity.ast.infile

import verity.parsing._

trait Type extends Node

case class TypeRef(name: Name, args: Seq[Type]) extends Type {
  def text = if (args.isEmpty) name.text else s"$name<${args.mkString(",")}>"

  override def textRange: TextRange = ???
}

class TypeRange(var upperBound: Type, var lowerBound: Type) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}

case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
  def text = s"(${typ.text})"
}

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}

//TODO arrays
class ArrayType

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  def text = ???

  override def textRange: TextRange = ???
}