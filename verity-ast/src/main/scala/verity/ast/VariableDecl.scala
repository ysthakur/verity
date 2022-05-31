package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

/** A variable declaration (local variable or field)
  */
trait VariableDecl extends Tree, HasText, HasType, NamedTree, HasModifiers {
  def name: String

  /** What it gets initialized to, unless it's just declared
    * @return
    */
  def initExpr: Option[ResolvedOrUnresolvedExpr]

  /** Whether or not this is simply a declaration
    * @return True if only a declaration, false if also intialized
    */
  def declarationOnly: Boolean = initExpr == None

  def isFinal: Boolean
}

class Field(
  val fieldName: Text,
  override val modifiers: ArrayBuffer[Modifier],
  var typ: Type,
  var initExpr: Option[ResolvedOrUnresolvedExpr] = None,
  override val isFinal: Boolean
) extends VariableDecl,
      ClassChild,
      HasModifiers,
      HasType {
  override def name = fieldName.text
  override def text =
    s"${Modifier.modifiersText(modifiers)} ${typ.text} ${fieldName.text}${initExpr.fold("")("=" + _.text)};"
  // override def textRange = ???
}

class LocalVar(
  override val modifiers: Iterable[Modifier],
  val varName: Text,
  var typ: Type,
  var initExpr: Option[ResolvedOrUnresolvedExpr] = None,
  val endInd: Int,
  override val isFinal: Boolean
) extends VariableDecl,
      Statement {

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar =>
      name == other.name && typ == other.typ && this.modifiers.toSet == other.modifiers.toSet
    case _ => false
  }
  override def name: String = varName.text
  override def text: String = {
    s"${HasText.seqText(modifiers)} $typ $name${initExpr.fold("")("=" + _.text)};"
  }
  override def textRange = ???
}
