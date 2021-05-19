package verity.ast.infile

import verity.ast.{NamedTree, Text, TextRange}

trait TypeDef extends NamedTree {

  /** Get a reference to this type parameter
    */
  def makeRef(path: Seq[Text], typeArgs: TypeArgList): ResolvedTypeRef =
    ResolvedTypeRef(path, typeArgs, this)

  def makeRef(path: Seq[Text]): ResolvedTypeRef =
    ResolvedTypeRef(path, TypeArgList(typeParams.params.map(_.makeRef), TextRange.synthetic), this)

  def makeRef: ResolvedTypeRef =
    makeRef(
      Text(this.name) :: Nil,
      TypeArgList(typeParams.params.map(_.makeRef), TextRange.synthetic)
    )

  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def superTypes: Iterable[Type]

  def typeParams: TypeParamList

  //todo work on this
  def strictSubTypeDefOf(sup: TypeDef): Boolean =
    this != sup && superTypes.exists {
      case `sup`                          => true
      case ResolvedTypeRef(_, _, typeDef) => typeDef.strictSubTypeDefOf(sup)
      case unresolved.UnresolvedTypeRef(_, _, typeDef) =>
        typeDef.fold(false)(_.strictSubTypeDefOf(sup))
    }
}
