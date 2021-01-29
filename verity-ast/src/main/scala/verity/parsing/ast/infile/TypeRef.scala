package verity.parsing.ast.infile

import verity.parsing._

type TypeArg = TypeRef | Wildcard

class TypeRef(name: ValidId, args: List[TypeArg]) extends Node {
  def text = ???

  override def textRange: TextRange = ???
}

class Wildcard(upper: TypeRef | Null, lower: TypeRef | Null)
