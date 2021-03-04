package verity.ast.infile

import verity.parsing.TextRange
import verity.ast.infile.Expr

class LocalVar(
    override val name: String,
    override val textRange: TextRange,
    var myType: Type,
    var initExpr: Option[Expr] = None,
    val isFinal: Boolean) extends VariableDecl {

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar => name == other.name && myType == other.myType && isFinal == other.isFinal
    case _ => false
  }
  override def text: String = {
    val sb = StringBuilder(if (isFinal) "final " else "").append(name)
    if (initExpr != None) sb.append('=').append(initExpr.get.text).append(';')
    sb.toString
  }
}