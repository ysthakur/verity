package verity.parsing.ast.infile

<<<<<<< HEAD:verity-ast/src/main/scala/verity/parsing/ast/infile/LocalVar.scala
import verity.parsing.TextRange
import verity.parsing.ast.infile.expr.Expr
=======
import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.expr.Expr
>>>>>>> 46a4767a1d9bea055c8ac44bf426b91d71bc79b1:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/LocalVar.scala

class LocalVar(override val name: String,
               override val textRange: TextRange,
               var myType: ITypeRef,
               var initExpr: Option[Expr] = None,
               val isFinal: Boolean = false) extends IVariableDecl {

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