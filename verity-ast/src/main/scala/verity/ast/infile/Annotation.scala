package verity.ast.infile

import verity.ast._
// import verity.ast.ToJava.given

case class Annotation(name: String, args: ArgList, appliedTo: Tree, startOffset: Int) extends HasText {
  override def text: String = s"@$name$args"
  override def textRange = TextRange(startOffset, args.textRange.end)
}

object Annotation {
  // given ToJava[Annotation] = annot => s"@${annot.name.toJava}${annot.args.toJava}"
}

trait HasAnnotations {
  def annotations: Iterable[Annotation]
}