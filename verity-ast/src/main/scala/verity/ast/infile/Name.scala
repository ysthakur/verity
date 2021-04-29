package verity.ast.infile

import verity.ast.*

//Make constructor private[verity.parsing]
// case class Name private[verity] (text: String, override val textRange: TextRange)
//     extends Tree, HasText {
//   override def toString = text
// }

trait NamedTree extends Tree {
  def name: String
}
