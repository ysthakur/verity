package verity.ast.infile

import verity.parsing.TextRange
import verity.ast.{ParentNode, INode}

import scala.collection.mutable.ListBuffer

trait TypeDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    metaclass: TypeDefType,
    name: String,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    textRange: TextRange
) extends Node
    with HasModifiers
    with TypeRepr {
  override def text: String = ???
    // s"${modifiers.text} ${metaclass.text} $name { ${children.map(_.text).mkString(" ")}}"
}

enum TypeDefType(val text: String) extends INode {
  case CLASS extends TypeDefType("class")
  case INTERFACE extends TypeDefType("interface")
  case ENUM extends TypeDefType("enum")
}

case class ClassDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    metaclass: TypeDefType,
    name: String,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    override val textRange: TextRange
) extends TypeDef(annotations, modifiers, TypeDefType.CLASS, name, fields, methods, textRange) {

}