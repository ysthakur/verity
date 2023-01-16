package verity.compiler.ast

import cats.data.NonEmptyList

sealed trait TypeDef extends Def {

  /** The type parameters and const parameters for this typedef. None of the
    * param lists should be normal or given parameters.
    */
  def comptimeParams: ComptimeParams = ComptimeParams(Nil, Nil, Nil)

  def fields: List[Field] = Nil
}

case class Field(name: String, typ: Type) extends Tree

case class Record(
  name: String,
  override val comptimeParams: ComptimeParams,
  params: Params
) extends TypeDef

case class EnumDef(
  name: String,
  override val comptimeParams: ComptimeParams,
  params: Params,
  cases: List[EnumCase]
) extends TypeDef

/** A particular case/variant of an enum
  * @param name
  *   The name of the case
  * @param comptimeParams
  *   Compile-time parameters for this particular case
  * @param params
  *   Runtime parameters for this particular case
  * @param ctorComptimeArgs
  *   The compile-time arguments to pass to the upper enum's constructor, if
  *   any.
  * @param ctorNormArgs
  *   The normal arguments to pass to the upper enum's constructor, if any
  * @param ctorGivenArgs
  *   The implicit arguments to pass to the upper enum's constructor, if any
  */
case class EnumCase(
  name: String,
  comptimeParams: ComptimeParams,
  params: Params,
  ctorComptimeArgs: ComptimeArgs,
  ctorArgs: Args
)

/** A type alias
  *
  * @param name
  * @param comptimeParamss
  *   All the lists of compile-time parameters for this type alias. A
  *   compile-time parameter list can be either a type parameter list or a const
  *   parameter list.
  * @param body
  */
case class TypeAlias(
  override val name: String,
  override val comptimeParams: ComptimeParams,
  val body: Type
) extends TypeDef

object BuiltinTypes {
  def verityPkg(using root: Package): Package = root.findChild("verity").get

  def boolType(using root: Package): TypeDef = verityPkg.findType("Bool").get

  def intType(using root: Package): TypeDef = verityPkg.findType("Int").get

  def doubleType(using root: Package): TypeDef =
    verityPkg.findType("Double").get

  def charType(using root: Package): TypeDef = verityPkg.findType("Char").get

  def stringType(using root: Package): TypeDef =
    verityPkg.findType("String").get
}

/** todo make a copy for every root */
object NotGivenDef extends TypeDef {
  override def name = "NotGiven"
}

object NotProvenDef extends TypeDef {
  override def name = "NotProven"
}

/** A compile-time parameter list. Can be either a type parameter list or a
  * const parameter list (const parameters are values, but given at compile-time
  * instead of runtime)
  */
enum ComptimeParamList {
  case TypeParamList(params: List[TypeParam])
  case ConstParamList(params: List[Param], isGiven: Boolean)
}

/** @param name
  *   The parameter's name
  * @param upperBound
  * @param lowerBound
  * @param nameRange
  *   The TextRange of the name
  */
case class TypeParam(
  override val name: String,
  nameRange: TextRange = TextRange.synthetic
) extends TypeDef

case class ComptimeParams(
  typeParams: List[TypeParam],
  normConstParams: List[Param],
  givenConstParams: List[Param],
)

object ComptimeParams {
  def empty = ComptimeParams(Nil, Nil, Nil)
}
