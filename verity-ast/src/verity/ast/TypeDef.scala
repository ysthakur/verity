package verity.ast

import verity.ast.*

trait TypeDef extends Def {

  /** The type parameters and proof parameters for this typedef. None of the
    * param lists should be normal or given parameters.
    */
  def typeParams: TypeParamList = TypeParamList(Nil)
  def givenParams: ValParamList = ValParamList(Nil)
  def proofParams: ValParamList = ValParamList(Nil)

  def props: List[Prop] = Nil
}

case class Prop(name: String, typ: Type, value: Option[Expr]) extends Tree

case class Singleton(name: String, superTypes: List[Type], props: List[Prop])

case class VClass(name: String, superTypes: List[Type], override val props: List[Prop], isTrait: Boolean) extends TypeDef

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

case class TypeAlias(
  override val name: String,
  override val typeParams: TypeParamList,
  override val givenParams: ValParamList,
  override val proofParams: ValParamList,
  val body: Type
) extends TypeDef
