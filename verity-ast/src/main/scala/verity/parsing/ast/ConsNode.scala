package verity.parsing.ast

import verity.parsing.TextRange
import verity.parsing.ast.infile.{EmptyNode, Node}

case class ConsNode[N1, N2](n1: N1, n2: N2/* , override val textRange: TextRange */)/* extends Node*/ {

  //println(s"\n______________________________\nCreated consnode, n1=$n1, n2=$n2")
  // override def flatten: Node = ConsNode(n1.flatten, n2.flatten)
  // override def text: String = s"${n1}${n2}"
  // override def textRange = TextRange(n1.textRange.start, n2.textRange.end)
}