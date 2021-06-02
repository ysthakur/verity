package verity.ast.infile.unresolved

import verity.ast.infile._
import verity.ast.{Text, TextRange}

import scala.collection.mutable.ArrayBuffer

trait UnresolvedMethod extends Method

class UnresolvedConstructor(
  val modifiers: ArrayBuffer[Modifier],
  val ctorName: Text,
  var params: ParamList,
  var givenParams: Option[ParamList],
  var proofParams: Option[ParamList],
  private val _body: Block
) extends UnresolvedMethod {
  def typeParams: TypeParamList = ???
  def returnType: Type = ???

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"

  def body: Option[Block] = Some(_body)

  def name: String = ctorName.text
  def nameRange = ctorName.textRange
}

class UnresolvedNormalMethod(
  val modifiers: ArrayBuffer[Modifier],
  val typeParams: TypeParamList,
  private var _returnType: Type,
  val methodName: Text,
  var params: ParamList,
  var givenParams: Option[ParamList],
  var proofParams: Option[ParamList],
  val body: Option[Block]
) extends UnresolvedMethod {
  override def text =
    s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body.fold(";")(_.text)}"

  def returnType: Type = _returnType

  private[verity] def returnType_=(typ: Type): Unit = _returnType = typ

  def name: String = methodName.text
  def nameRange = methodName.textRange
}
