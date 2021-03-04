package verity.ast.infile

import verity.parsing.TextRange
import verity.ast.INode
import verity.ast.infile.Node

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Node {
  def modifiers: Iterable[Modifier]
  def modifiersText: String = modifiers.mkString(" ")
}

case class Modifier(modType: ModifierType, override val textRange: TextRange) extends Node {
  override def text: String = modType.toString.toLowerCase.nn
}

enum ModifierType extends INode {
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