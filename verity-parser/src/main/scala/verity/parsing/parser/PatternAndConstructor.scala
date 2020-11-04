package verity.parsing.parser

import verity.parsing.ast.infile.Node
import verity.parsing._

case class PatternAndConstructor[N <: Node](
    pattern: Pattern, ctor: Node => N
  ) extends Pattern {

//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def apply(input: Reader): ParseResult =
    pattern.tryMatch(input) match {
      case Matched(create, rest, range) => Matched(() => ctor(create()), rest, range)
      case failed => failed
    }
}