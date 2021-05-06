package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

/**
  * A variable declaration (local variable or field)
  */
trait VariableDecl extends Tree, HasText, HasType, HasModifiers, NamedTree {
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

  def isGiven = this.hasModifier(ModifierType.GIVEN)
  def isProof = this.hasModifier(ModifierType.PROOF)
}

class Field(
    val fieldName: Text,
    override val modifiers: ListBuffer[Modifier],
    override var typ: Type,
    override val initExpr: Option[Expr] = None
) extends VariableDecl,
      ClassChild,
      HasModifiers,
      HasType {
  def isStatic: Boolean = this.hasModifier(ModifierType.STATIC)

  override def name = fieldName.text
  override def text: String = ??? //s"${fieldName.text}"
  override def textRange = ???
}

case class LocalVar(
    val modifiers: Iterable[Modifier],
    val varName: Text,
    var typ: Type,
    override val initExpr: Option[Expr] = None,
    val isFinal: Boolean,
    val endInd: Int
) extends VariableDecl, Statement {
  
  def name = varName.text
  
  override def text: String = {
    val sb = StringBuilder(if (isFinal) "final " else "").append(typ.text).append(name)
    if (initExpr != None) sb.append('=').append(initExpr.get.text).append(';')
    sb.toString
  }
  override def textRange = ???

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar =>
      this.name == other.name && this.typ == other.typ && this.isFinal == other.isFinal
    case _ => false
  }
}

object LocalVar {
  def unapply(lv: LocalVar): (String, Type, Option[Expr]) =
    (lv.name, lv.typ, lv.initExpr)
}
