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
  override def typeParams: TypeParamList = ???
  override def returnType: Type = ???
  override def proofs = Nil

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"

  override def body = Some(_body)

  override def name = ctorName.text
  def nameRange = ctorName.textRange
}

class UnresolvedNormalMethod(
  val modifiers: ArrayBuffer[Modifier],
  val typeParams: TypeParamList,
  private var _returnType: Type,
  val proofs: collection.immutable.ArraySeq[Type],
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
