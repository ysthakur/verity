package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

/**
 * Base trait for classes, interfaces, enums, and annotations
 */
sealed trait Classlike(val defType: ClasslikeType) extends NamedTree, TypeDef, HasText, HasModifiers, HasAnnotations {
  def modifiers: ListBuffer[Modifier]
  def typeParams: TypeParamList
  def fields: Iterable[Field]
  def methods: Iterable[Method]
  /**
   * The TextRange for the "class", "interface", or "enum" keyword token.
   */
  def metaclassTokTR: TextRange
  /**
   * The TextRange of the braces
   */
  def bodyRange: TextRange

  def children: Iterable[ClassChild]

  override def textRange = TextRange(
      if (annotations.nonEmpty) annotations.head.textRange.start
      else if (modifiers.nonEmpty) modifiers.head.textRange.start
      else metaclassTokTR.start,
      bodyRange.end
  )

  /** Find members inside a class given by a path, e.g. List("foo") to access field foo
    * @param cls The class inside which the fields/methods/classes to be found lie
    * @param path A nonempty Iterable denoting the path of the member
    */
  def findMember(path: Iterable[String]): Option[ClassChild] = {
    val childName = path.head
    val rest = path.tail

    val child = children.view.find(_.name == childName)

    if (rest.isEmpty) child
    else child.collect { case c: Classlike => c.findMember(rest) }.flatten
  }

  def methodGroups: Iterable[MethodGroup] =
    methods.groupBy(_.name).map(MethodGroup(_, _))
}

trait ClassChild extends NamedTree

trait HasCtors extends Classlike {
  def ctors: Iterable[Constructor]
  private[verity] def addCtor(ctor: Constructor): Unit
}

enum ClasslikeType(val text: String) extends Tree {
  case CLASS extends ClasslikeType("class")
  case INTERFACE extends ClasslikeType("interface")
  case ENUM extends ClasslikeType("enum")
}

/**
 * A class definition (not an interface, enum, or annotation)
 * @param metaclassTokTR The `TextRange` of the "class" keyword token
 * @param bodyRange The `TextRange` of the braces
 */
case class ClassDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    name: String,
    typeParams: TypeParamList,
    fields: ListBuffer[Field],
    ctors: ListBuffer[Constructor],
    normMethods: ListBuffer[NormMethod],
    metaclassTokTR: TextRange,
    bodyRange: TextRange
) extends Classlike(ClasslikeType.CLASS), HasCtors {

  def methods = ctors ++ normMethods
  def children = fields ++ methods

  private[verity] def addCtor(ctor: Constructor) = ctors += ctor

  //todo also add fields (no need to preserve order)
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} class $name { ${methods.view.map(_.text).mkString(" ")}}"
}

case class InterfaceDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    name: String,
    typeParams: TypeParamList,
    fields: ListBuffer[Field],
    methods: ListBuffer[Method],
    metaclassTokTR: TextRange,
    bodyRange: TextRange
) extends Classlike(ClasslikeType.INTERFACE) {
  def children = fields ++ methods
  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} interface $name { ${methods.mkString(" ")}}"
}

//TODO parse enums
case class EnumDef(
    annotations: ListBuffer[Annotation],
    modifiers: ListBuffer[Modifier],
    name: String,
    typeParams: TypeParamList,
    constants: List[EnumConstant],
    fields: ListBuffer[Field],
    ctors: ListBuffer[Constructor],
    methods: ListBuffer[Method],
    metaclassTokTR: TextRange,
    bodyRange: TextRange
) extends Classlike(ClasslikeType.ENUM), HasCtors {
  def children = (constants ++ fields: Iterable[ClassChild]) ++ methods

  private[verity] def addCtor(ctor: Constructor) = ctors += ctor

  override def text: String =
    s"${modifiers.map(_.text).mkString(" ")} enum $name { ${methods.mkString(" ")}}"
}

case class EnumConstant(name: String) extends ClassChild
