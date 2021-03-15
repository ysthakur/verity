package verity.ast.infile

import verity.ast.Tree
import verity.parsing.{TextRange, HasText}

//Make constructor private[verity.parsing]
case class Name private[verity] (text: String, override val textRange: TextRange) extends Tree, HasText {
  override def toString = text
}