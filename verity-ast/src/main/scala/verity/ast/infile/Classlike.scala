package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

/**
 * Base trait for classes, interfaces, enums, and annotations
 */
sealed trait Classlike extends NamedTree, TypeDef, HasText, HasModifiers, HasAnnotations {
  def modifiers: ListBuffer[Modifier]
  def defType: ClasslikeType
  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def bodyRange: TextRange

  def children: Iterable[Classlike.ClassChild]

  override def textRange = TextRange(
      if (annotations.nonEmpty) annotations.head.textRange.start
      else if (modifiers.nonEmpty) modifiers.head.textRange.start
      else defType.textRange.start,
      bodyRange.end
  )

  /** Find members inside a class given by a path, e.g. List("foo") to access field foo
    * @param cls The class inside which the fields/methods/classes to be found lie
    * @param path A nonempty Iterable denoting the path of the member
    */
  def findMember(path: Iterable[String]): Option[Classlike.ClassChild] = {
    val childName = path.head
    val rest = path.tail

    val child = children.view.find(_.name.toString == childName)

    if (rest.isEmpty) child
    else child.collect { case c: Classlike => c.findMember(rest) }.flatten
  }
}
object Classlike {
  type ClassChild = Classlike | MethodGroup | Method | Field | EnumConstant
}

//TODO don't store a TextRange here
enum ClasslikeType(val text: String, val textRange: TextRange) extends Tree {
  case CLASS(override val textRange: TextRange) extends ClasslikeType("class", textRange)
  case INTERFACE(override val textRange: TextRange) extends ClasslikeType("interface", textRange)
  case ENUM(override val textRange: TextRange) extends ClasslikeType("enum", textRange)
}

case class ClassDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: ClasslikeType,
    name: String,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends Classlike {
  def ctors: Iterable[Method] = methods.filter(_.isCtor)

  def children = fields ++ methods

  //todo also add fields (no need to preserve order)
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} ${defType.text} $name { ${methods.view.map(_.text).mkString(" ")}}"
}

case class InterfaceDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: ClasslikeType,
    name: String,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends Classlike {
  def children = fields ++ methods
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} ${defType.text} $name { ${methods.mkString(" ")}}"
}

//TODO parse enums
case class EnumDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    defType: ClasslikeType,
    name: String,
    constants: List[EnumConstant],
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    bodyRange: TextRange
) extends Classlike {
  def children = (constants ++ fields: Iterable[Classlike.ClassChild]) ++ methods
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} ${defType.text} $name { ${methods.mkString(" ")}}"
}

case class EnumConstant(name: String) extends NamedTree
