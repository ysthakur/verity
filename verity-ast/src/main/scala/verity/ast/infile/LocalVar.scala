package verity.ast.infile

import verity.ast.infile.Expr

class LocalVar(
    override val name: String,
    var myType: Type,
    var initExpr: Option[Expr] = None,
    val isFinal: Boolean,
    val endInd: Int) extends VariableDecl, Statement {

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar => name == other.name && myType == other.myType && isFinal == other.isFinal
    case _ => false
  }
  override def text: String = {
    val sb = StringBuilder(if (isFinal) "final " else "").append(myType.text).append(name)
    if (initExpr != None) sb.append('=').append(initExpr.get.text).append(';')
    sb.toString
  }
  override def textRange = ???
}