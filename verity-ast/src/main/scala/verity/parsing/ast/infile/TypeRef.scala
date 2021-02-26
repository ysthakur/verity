package verity.parsing.ast.infile

import verity.parsing._

type Type = TypeRef | Wildcard

case class TypeRef(name: ValidId, args: List[TypeArg]) extends Node {
  def text = ???

  override def textRange: TextRange = ???
}

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef])
