package verity.ast

import verity.ast.*

trait TypeDef extends Def {
  /**
  * The type parameters and proof parameters for this typedef. None of the param lists should be normal or given parameters.
  */
  def paramLists: List[TypeParamList | ValParamList] = Nil
}

object BuiltinTypes {
  def verityPkg(using root: Package): Package = root.findChild("verity").get

  def boolType(using root: Package): TypeDef = verityPkg.findType("Bool").get

  def intType(using root: Package): TypeDef = verityPkg.findType("Int").get

  def doubleType(using root: Package): TypeDef = verityPkg.findType("Double").get

  def charType(using root: Package): TypeDef = verityPkg.findType("Char").get

  def stringType(using root: Package): TypeDef = verityPkg.findType("String").get
}

/** todo make a copy for every root */
object NotGivenDef extends TypeDef {
  def name = "NotGiven"
}

object NotProvenDef extends TypeDef {
  def name = "NotProven"
}

case class TypeParam(override val name: String) extends TypeDef

case class TypeParamList(params: List[TypeParam]) extends Tree

case class TypeAlias(
  override val name: String,
  override val paramLists: List[TypeParamList | ValParamList]
) extends TypeDef
