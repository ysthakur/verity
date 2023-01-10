package verity.compiler.ast

import cats.data.NonEmptyList

sealed trait TypeDef extends Def {

  /** The type parameters and proof parameters for this typedef. None of the
    * param lists should be normal or given parameters.
    */
  def constParamss: List[ConstParamList] = Nil
  def valParamss: List[ValParamList] = Nil
}

case class Prop(name: String, typ: Type, value: Option[Expr]) extends Tree

case class Record(
  name: String,
  override val constParamss: List[ConstParamList],
  override val valParamss: List[ValParamList]
) extends TypeDef

case class EnumDef(
  name: String,
  override val constParamss: List[ConstParamList],
  override val valParamss: List[ValParamList],
  cases: List[EnumCase]
) extends TypeDef

/** A particular case/variant of an enum
  * @param name
  *   The name of the case
  * @param constParamss
  *   Compile-time parameters for this particular case
  * @param valParamss
  *   Parameters for this particular case
  * @param ctorConstArgss
  *   The compile-time arguments to pass to the upper enum's constructor, if any
  * @param ctorValArgss
  *   The arguments to pass to the upper enum's constructor, if any
  */
case class EnumCase(
  name: String,
  constParamss: List[ConstParamList],
  valParamss: NonEmptyList[ValParamList],
  ctorConstArgss: List[ConstArgList],
  ctorValArgss: List[ValArgList]
)

case class TypeAlias(
  override val name: String,
  override val constParamss: List[ConstParamList],
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

enum ConstParamList {
  case TypeParamList(params: List[TypeParam])
  case ProofParamList(params: List[ValParam])
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
  upperBound: Option[Type],
  lowerBound: Option[Type],
  nameRange: TextRange
) extends TypeDef

case class TypeParamList(params: List[TypeParam]) extends Tree
