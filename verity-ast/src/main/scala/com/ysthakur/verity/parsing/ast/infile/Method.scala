package com.ysthakur.verity.parsing.ast.infile

<<<<<<< HEAD:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/Method.scala
import com.ysthakur.verity.parsing.TextRange
=======
>>>>>>> master:javamm-ast/src/main/scala/com/ysthakur/javamm/parsing/ast/infile/Method.scala
import com.ysthakur.verity.parsing.ast.infile.expr.Expr

import scala.collection.mutable.ListBuffer

case class Method(
    val modifiers: ModifierList,
    returnType: TypeRef | Null,
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