package verity.parsing.parser

import verity.parsing.ast.infile.Node
import verity.parsing._

case class PatternAndConstructor[I, N](
    pattern: Pattern.Aux[I], 
    ctor: I => N
  ) extends Pattern {
  type Out = N
//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def apply(input: Reader): ParseResult[N] =
    pattern.tryMatch(input) match {
      case Matched(create, rest, range) => Matched(() => ctor(create()), rest, range)
      case failed: Failed => failed
    }
}