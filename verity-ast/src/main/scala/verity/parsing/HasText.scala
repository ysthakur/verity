package verity.parsing

//TODO Replace with a typeclass?
trait HasText {
  def text: String
  def textRange: TextRange
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