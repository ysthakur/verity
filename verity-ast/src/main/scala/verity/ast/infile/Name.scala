package verity.ast.infile

import verity.ast.Tree
import verity.parsing.{TextRange, HasText}

case class Name private[verity] (text: String, override val textRange: TextRange) extends Tree, HasText {
}