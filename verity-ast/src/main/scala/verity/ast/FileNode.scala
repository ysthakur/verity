package verity.ast

import verity.parsing.HasText
import verity.ast.infile._

import scala.collection.mutable.ListBuffer

class FileNode(packageRef: DotRef, imports: ListBuffer[Import], children: ListBuffer[TemplateDef]) {
  def text: String = ???
}
