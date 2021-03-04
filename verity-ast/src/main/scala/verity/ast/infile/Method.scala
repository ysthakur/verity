package verity.ast.infile

import verity.parsing.TextRange
import verity.ast.infile.Expr

import scala.collection.mutable.ListBuffer

case class Method(
    override val modifiers: ListBuffer[Modifier],
    returnType: TypeRef,
    name: String,
    ctparams: CTParamList,
    params: ParamList,
    private var _body: Option[Block], /*Option[Block|Expr]*/
    override val textRange: TextRange
) extends MethodLike, HasModifiers {
  def text: String = ???
  override def body: Option[Block] /*Option[Block|Expr]*/ = _body
  def body_=(newBody: Option[Block]/*Option[Block|Expr]*/): Unit = _body = newBody
}