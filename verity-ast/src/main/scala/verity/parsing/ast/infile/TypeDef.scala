package verity.parsing.ast.infile

<<<<<<< HEAD:verity-ast/src/main/scala/verity/parsing/ast/infile/TypeDef.scala
import verity.parsing.TextRange
import verity.parsing.ast.{ParentNode, INode}
=======
import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.{ParentNode, INode}
>>>>>>> 46a4767a1d9bea055c8ac44bf426b91d71bc79b1:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/TypeDef.scala

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