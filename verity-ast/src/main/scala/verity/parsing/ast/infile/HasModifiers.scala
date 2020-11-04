package verity.parsing.ast.infile

import verity.parsing.TextRange
import verity.parsing.ast.{INode, Node}

import scala.collection.mutable.ListBuffer

trait HasModifiers extends Node {
  def modifiers: ModifierList
  def modifiersText: String = modifiers.modifiers.mkString(" ")
}

//TODO decide if this should be replaced with a plain ListBuffer[Modifier]
case class ModifierList(modifiers: ListBuffer[Modifier], override val textRange: TextRange) extends Node {
  override def text: String = modifiers.mkString(" ")
}

case class Modifier(modType: ModifierType, override val textRange: TextRange) extends Node {
  override def text: String = modType.toString.toLowerCase
}

enum ModifierType extends INode {
  case PUBLIC, PRIVATE, PROTECTED, DEFAULT, STATIC, ABSTRACT, FINAL, NATIVE, TRANSIENT, VOLATILE, SYNCHRONIZED, CONST
}