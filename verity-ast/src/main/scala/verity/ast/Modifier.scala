package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

trait HasModifiers extends Tree {
  def modifiers: Iterable[Modifier]
  def modifiersText: String = modifiers.mkString(" ")

  def accessModifier: Option[ModifierType] =
    this.modifiers.view.map(_.modType).find(ModifierType.accessModifiers)

  def isGiven: Boolean = this.hasModifier(ModifierType.GIVEN)

  def isProof: Boolean = this.hasModifier(ModifierType.PROOF)

  def hasModifier(modType: ModifierType) = this.modifiers.exists(_.modType == modType)

  def isAbstract: Boolean = this.hasModifier(ModifierType.ABSTRACT)
  def isStatic: Boolean = this.hasModifier(ModifierType.STATIC)
}

case class Modifier(modType: ModifierType, override val textRange: TextRange)
    extends Tree,
      HasTextRange {
  override def text: String = modType.toString.toLowerCase.nn
}
object Modifier {
  def modifiersText(modifiers: Iterable[Modifier]) = HasText.seqText(modifiers, "", " ", "")
}

enum ModifierType extends Tree {
  case PUBLIC, PRIVATE, PROTECTED, DEFAULT, STATIC, ABSTRACT, FINAL, NATIVE, TRANSIENT, VOLATILE,
  SYNCHRONIZED, CONST, GIVEN, PROOF
}

object ModifierType {
  val accessModifiers: Set[ModifierType] = Set(
    ModifierType.PUBLIC,
    ModifierType.PRIVATE,
    ModifierType.PROTECTED
  )
}
