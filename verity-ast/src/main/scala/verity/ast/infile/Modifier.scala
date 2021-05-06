package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Tree {
  def modifiers: Iterable[Modifier]
  def modifiersText: String = modifiers.mkString(" ")

  def hasModifier(modType: ModifierType) = this.modifiers.exists(_.modType == modType)

  def accessModifier: Option[ModifierType] =
    this.modifiers.view.map(_.modType).find(ModifierType.accessModifiers)
}

case class Modifier(modType: ModifierType, override val textRange: TextRange) extends Tree, HasText {
  override def text: String = modType.toString.toLowerCase.nn
}
object Modifier {
  def modifiersText(modifiers: Iterable[Modifier]) = HasText.seqText(modifiers, "", " ", "")
}

enum ModifierType extends Tree {
  case
    PUBLIC,
    PRIVATE,
    PROTECTED,
    DEFAULT,
    STATIC,
    ABSTRACT,
    FINAL,
    NATIVE,
    TRANSIENT,
    VOLATILE,
    SYNCHRONIZED,
    CONST,
    GIVEN,
    PROOF
}

object ModifierType {
  val accessModifiers: Set[ModifierType] = Set(
    ModifierType.PUBLIC,
    ModifierType.PRIVATE,
    ModifierType.PROTECTED
  )
}