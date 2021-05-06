package verity.ast.infile

import verity.ast.{Text, NamedTree, TextRange}

trait TypeDef extends NamedTree {
  /**
   * Get a reference to this type parameter
   */
  def makeRef(typeArgs: TypeArgList = Empty[TypeArgList]): TypeRef =
  	TypeRef(Text(this.name) :: Nil, typeArgs, Some(this))

  def fields: Iterable[Field]
  def methods: Iterable[Method]
}
