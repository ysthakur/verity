package verity.parsing

//TODO make this a typeclasses
trait HasText {
  def text: String
  def textRange: TextRange
}