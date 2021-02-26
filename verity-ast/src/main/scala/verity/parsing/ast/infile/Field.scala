package verity.parsing.ast.infile

import verity.parsing.TextRange
import verity.parsing.ast.infile.Expr

import scala.collection.mutable.ListBuffer

class Field(
    override val name: String,
    typeRef: Option[TypeRef] = None,
    var myType: ITypeRef,
    override val modifiers: ModifierList,
    var initExpr: Option[Expr] = None,
    override val textRange: TextRange
) extends IVariableDecl
    with HasModifiers
    with HasType {
  override def text: String = ???
}
