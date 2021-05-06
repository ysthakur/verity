package verity.ast.infile

import verity.ast.{Text, NamedTree, TextRange}

trait TypeDef extends NamedTree {
  /**
   * Get a reference to this type parameter
   */
  def makeRef(typeArgs: Seq[Type] = Nil): TypeRef =
  	TypeRef(Text(this.name), typeArgs, Some(this), TextRange.synthetic)

  def fields: Iterable[Field]
  def methods: Iterable[Method]
}
