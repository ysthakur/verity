package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

/**
  * A variable declaration (local variable or field)
  */
trait VariableDecl extends Tree, HasText, HasType, NamedTree {
  def name: String
  /**
    * What it gets initialized to, unless it's just declared
    * @return
    */
  def initExpr: Option[Expr]

  /**
    * Whether or not this is simply a declaration
    * @return True if only a declaration, false if also intialized
    */
  def declarationOnly: Boolean = initExpr == None
}

class Field(
    override val name: String,
    override val modifiers: ListBuffer[Modifier],
    var typ: Type,
    var initExpr: Option[Expr] = None
) extends VariableDecl,
      ClassChild,
      HasModifiers,
      HasType {
  override def text: String = ???
  override def textRange = ???
}

class LocalVar(
    override val name: String,
    var typ: Type,
    var initExpr: Option[Expr] = None,
    val isFinal: Boolean,
    val endInd: Int) extends VariableDecl, Statement {

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar => name == other.name && typ == other.typ && isFinal == other.isFinal
    case _ => false
  }
  override def text: String = {
    val sb = StringBuilder(if (isFinal) "final " else "").append(typ.text).append(name)
    if (initExpr != None) sb.append('=').append(initExpr.get.text).append(';')
    sb.toString
  }
  override def textRange = ???
}
