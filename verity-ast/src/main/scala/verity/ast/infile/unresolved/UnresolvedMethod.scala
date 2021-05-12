package verity.ast.infile.unresolved

import verity.ast.infile._
import verity.ast.{Text, TextRange}

import scala.collection.mutable.ListBuffer

trait UnresolvedMethod extends Method

class UnresolvedConstructor(
    val modifiers: ListBuffer[Modifier],
    val ctorName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    private val _body: Block
) extends UnresolvedMethod {
  def typeParams: TypeParamList = ???
  def returnType: Type = ???

  def body: Option[Block] = Some(_body)

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"

  def name: String = ctorName.text
  def nameRange = ctorName.textRange
}

class UnresolvedNormMethod(
    val modifiers: ListBuffer[Modifier],
    val typeParams: TypeParamList,
    private var _returnType: Type,
    val methodName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    val body: Option[Block]
) extends UnresolvedMethod {
  override def text = s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body
    .fold(";")(_.text)}"

  def returnType: Type = _returnType

  private[verity] def returnType_=(typ: Type): Unit = _returnType = typ

  def name: String = methodName.text
  def nameRange = methodName.textRange
}
