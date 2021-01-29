package verity.parsing.ast.infile

<<<<<<< HEAD:verity-ast/src/main/scala/verity/parsing/ast/infile/Method.scala
import verity.parsing.TextRange
import verity.parsing.ast.infile.expr.Expr
=======
import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.expr.Expr
>>>>>>> 46a4767a1d9bea055c8ac44bf426b91d71bc79b1:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/Method.scala

import scala.collection.mutable.ListBuffer

case class Method(
    val modifiers: ModifierList,
    returnType: TypeRef,
    name: String,
    ctparams: CTParamList,
    params: ParamList,
    private var _body: Option[Block], /*Option[Block|Expr]*/
    override val textRange: TextRange
) extends IMethodLike {
  def text: String = ???
  override def body: Option[Block] /*Option[Block|Expr]*/ = _body
  def body_=(newBody: Option[Block]/*Option[Block|Expr]*/): Unit = _body = newBody
}