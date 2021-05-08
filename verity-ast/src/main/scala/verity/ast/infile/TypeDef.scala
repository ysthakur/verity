package verity.ast.infile

import verity.ast.{Text, NamedTree, TextRange}

trait TypeDef extends NamedTree {
  /**
   * Get a reference to this type parameter
   */
  def makeRef(typeArgs: TypeArgList): ResolvedTypeRef =
  	ResolvedTypeRef(Text(this.name) :: Nil, typeArgs, this)

  def makeRef: ResolvedTypeRef =
    ResolvedTypeRef(Text(this.name) :: Nil, TypeArgList(typeParams.params.map(_.makeRef), TextRange.synthetic), this)

  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def superTypes: Iterable[Type]

  def typeParams: TypeParamList
}
