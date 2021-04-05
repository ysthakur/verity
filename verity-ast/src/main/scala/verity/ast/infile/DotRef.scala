package verity.ast.infile

import verity.ast.{Tree, ToJava}
import verity.parsing.{TextRange, HasText}
// import verity.codegen.ToJava

case class DotRef(path: Iterable[Name]) extends Tree, HasText derives ToJava {
  override def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
  override def text = path.map(_.text).mkString(".")
}

case class Test(foo: DotRef = new DotRef(Nil)) derives ToJava