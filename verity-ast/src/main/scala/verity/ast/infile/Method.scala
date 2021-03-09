package verity.ast.infile

import verity.ast.Tree
import verity.parsing._

import scala.collection.mutable.ListBuffer

trait MethodLike {
  def returnType: TypeRef
  def name: Name
  def params: ParamList
  
  def body: Option[Block] //Option[Block|Expr]
  def body_=(newBody: Option[Block] /*Option[Block|Expr]*/): Unit
}

case class Method(
    override val modifiers: ListBuffer[Modifier],
    returnType: TypeRef,
    name: Name,
    params: ParamList,
    private var _body: Option[Block],
    isCtor: Boolean
) extends MethodLike, HasModifiers {
  def text: String = ???
  override def body: Option[Block] = _body
  def body_=(newBody: Option[Block]): Unit = _body = newBody
}

case class Parameter(
  annotations: List[Annotation],
  paramType: Type,
  name: Name,
  isGiven: Boolean,
  isErased: Boolean,
  override val textRange: TextRange
) extends HasText, HasAnnotations, NamedTree {
  def text: String =
    (if (isGiven) "given " else "") +
    (if (isErased) "erased " else "") +
    s"${annotations.map(_.text).mkString(" ")} ${paramType.text} ${name.text}"
}

case class ParamList(params: List[Parameter], val textRange: TextRange) extends Tree, HasText {
  override def text: String = params.map(_.text).mkString("(", ",", ")")
}