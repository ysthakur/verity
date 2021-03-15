package verity.ast.infile

import verity.parsing.TextRange
import verity.ast.{ParentNode, Tree}
import verity.parsing.Token

import scala.collection.mutable.ListBuffer

//Base trait for classes, interfaces, enums, and annotations
sealed trait TemplateDef extends Node, HasModifiers, TypeRepr, HasAnnotations, NamedTree {
  def modifiers: ListBuffer[Modifier]
  def defType: TemplateDefType
  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def bodyRange: TextRange
}

enum TemplateDefType(val text: String, val textRange: TextRange) extends Tree {
  case CLASS(override val textRange: TextRange) extends TemplateDefType("class", textRange)
  case INTERFACE(override val textRange: TextRange) extends TemplateDefType("interface", textRange)
  case ENUM(override val textRange: TextRange) extends TemplateDefType("enum", textRange)
}

case class ClassDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: TemplateDefType,
    name: Name,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends TemplateDef {
  def ctors: Iterable[Method] = methods.filter(_.isCtor)

  //todo also add fields
  override def text: String =
    s"${modifiers.mkString(" ")} ${defType.text} $name { ${methods.mkString(" ")}}"
}

case class InterfaceDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: TemplateDefType,
    name: Name,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends TemplateDef {
  override def text: String = ???
    // s"${modifiers.text} ${metaclass.text} $name { ${children.map(_.text).mkString(" ")}}"
}