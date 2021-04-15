package verity.ast.infile

import verity.ast.{ParentNode, Tree}
import verity.parsing._

import scala.collection.mutable.ListBuffer

//Base trait for classes, interfaces, enums, and annotations
sealed trait Classlike extends Tree, HasText, HasModifiers, TypeDef, HasAnnotations, NamedTree {
  def modifiers: ListBuffer[Modifier]
  def defType: TemplateDefType
  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def bodyRange: TextRange

  override def textRange = TextRange(
    if (annotations.nonEmpty) annotations.head.textRange.start
    else if (modifiers.nonEmpty) modifiers.head.textRange.start
    else defType.textRange.start,
    bodyRange.end
  )
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
) extends Classlike {
  def ctors: Iterable[Method] = methods.filter(_.isCtor)

  //todo also add fields (no need to preserve order)
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} ${defType.text} $name { ${methods.view.map(_.text).mkString(" ")}}"
}

case class InterfaceDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: TemplateDefType,
    name: Name,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends Classlike {
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} ${defType.text} $name { ${methods.mkString(" ")}}"
}