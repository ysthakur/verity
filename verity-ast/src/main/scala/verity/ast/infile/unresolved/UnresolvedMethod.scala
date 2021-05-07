package verity.ast.infile.unresolved

import verity.ast.infile.*
import verity.ast.{Text, TextRange}

import scala.collection.mutable.ListBuffer

trait UnresolvedMethod extends Method

class UnresolvedConstructor(
    val modifiers: ListBuffer[Modifier],
    val ctorName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    val actualBody: UnresolvedBlock,
    val textRange: TextRange
) extends UnresolvedMethod {
  def typeParams: TypeParamList = ???
  def returnType: Type = ???

  def body: Option[Block] = None

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"

  def name: String = ctorName.text
}

class UnresolvedNormMethod(
    val modifiers: ListBuffer[Modifier],
    val typeParams: TypeParamList,
    private var _returnType: Type,
    val methodName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    val actualBody: Option[UnresolvedBlock],
    val textRange: TextRange
) extends UnresolvedMethod {
  override def text = s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body
    .fold(";")(_.text)}"
  def body: Option[Block] = None

  def returnType: Type = _returnType

  private[verity] def returnType_=(typ: Type): Unit = _returnType = typ

  def name: String = methodName.text
}
