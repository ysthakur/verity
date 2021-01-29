package verity.parsing.ast.infile

import verity.parsing.TextRange
import verity.parsing.ast.{ParentNode, INode}

import scala.collection.mutable.ListBuffer

case class TypeDef(
    modifiers: ModifierList,
    metaclass: TypeDefType,
    name: String,
    override val children: ListBuffer[Field | Method | Rule],
    override val textRange: TextRange
) extends Node
    with ParentNode
    with HasModifiers
    with TypeRepr {
  override type Child = Field | Method | Rule
  override def text: String =
    s"${modifiers.text} ${metaclass.text} $name { ${children.map(_.text).mkString(" ")}}"
}

enum TypeDefType(val text: String) extends INode {
  case CLASS extends TypeDefType("class")
  case INTERFACE extends TypeDefType("interface")
  case ENUM extends TypeDefType("enum")
}