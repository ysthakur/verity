package verity.ast

//TODO Replace with a typeclass?
trait HasText {
  def text: String
  def textRange: TextRange
  def isSynthetic: Boolean = this.textRange.isSynthetic
}

object HasText {
  /**
   * Convert an Iterable of trees with test to a single string
   */
  def seqText(iter: Iterable[HasText], sep: String = ",", start: String = "(", end: String = ")"): String =
    iter.view.map(_.text).mkString(start, sep, end)

  def optText(opt: Option[HasText], default: => String = ""): String =
    opt.fold(default)(_.text)
}

trait GetText[T] {
  def getText(tree: T): String
  extension (tree: T)
    inline def text: String = this.getText(tree)
}

object GetText {
  def apply[T](using gt: GetText[T]) = gt

  given optText[T : GetText]: GetText[Option[T]] = opt => opt.fold("")(_.text)

  given seqText[T : GetText]: GetText[Seq[T]] = seq => seq.view.map(_.text).mkString(" ")

  given hasTextText[T <: HasText]: GetText[T] = tree => tree.text
}