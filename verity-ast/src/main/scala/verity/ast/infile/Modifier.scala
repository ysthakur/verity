package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Tree {
  def modifiers: Iterable[Modifier]
  def modifiersText: String = modifiers.mkString(" ")
}

case class Modifier(modType: ModifierType, override val textRange: TextRange) extends Tree, HasText {
  override def text: String = modType.toString.toLowerCase.nn
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
    CONST
}