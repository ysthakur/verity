package verity.ast.infile

import verity.ast._
import verity.parsing.{TextRange, HasText}

case class Annotation(name: Name, args: ArgList, appliedTo: Tree, startOffset: Int) extends HasText {
  def text: String = s"@$name$args"
  def textRange = TextRange(startOffset, args.textRange.end)
}

trait HasAnnotations {
  def annotations: Iterable[Annotation]
}