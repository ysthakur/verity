package verity.ast.infile

import verity.ast.{NamedTree, Text, TextRange}

trait TypeDef extends NamedTree {

  /** Get a reference to this type parameter
    */
  def makeRef(path: Seq[Text], typeArgs: TypeArgList): ResolvedTypeRef =
    ResolvedTypeRef(path, typeArgs, this)

  def makeRef(path: Seq[Text]): ResolvedTypeRef =
    ResolvedTypeRef(path, TypeArgList(typeParams.params.map(_.makeRef), TextRange.synthetic), this)

  private lazy val cachedTypeRef: Option[ResolvedTypeRef] = Option.when(typeParams.params.isEmpty)(_makeRef)
  private def _makeRef: ResolvedTypeRef =
    makeRef(
      Text(this.name) :: Nil,
      TypeArgList(typeParams.params.map(_.makeRef), TextRange.synthetic)
    )
  def makeRef: ResolvedTypeRef = cachedTypeRef.getOrElse(_makeRef)

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

object TypeDef {
  val placeholder = new TypeDef {
    def name: String = "PLACEHOLDER"
    def fields: Iterable[Field] = Nil
    def methods: Iterable[Method] = Nil
    def superTypes: Iterable[Type] = Nil

    def typeParams: TypeParamList = new TypeParamList(Nil, TextRange.synthetic)
  }
}
