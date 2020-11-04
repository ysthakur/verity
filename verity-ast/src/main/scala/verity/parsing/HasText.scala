package verity.parsing

trait HasText {
  def text: CharSequence //= ???
  def startOffset: Int = ???
  def endOffset: Int = ??? //TODO REMOVE THESE!
}
