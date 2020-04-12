// package com.ysthakur.parsing.parser

// import com.ysthakur.parsing.grammar._

// import scala.reflect.ClassTag

// class SingleNodePattern[Node : ClassTag]
//     extends Pattern[Node] {

//   /**
//     * Whether or not it always matches the same input.
//     * If false, it might be a valid identifier or something
//     * that takes a variable length input or something like that,
//     * which means that what it contains must be stored.
//     */
//   override val isFixed: Boolean = false

//   override def tryMatch(input: List[Node]): ParseResult = {
//     if (input.size < 1) return NeedsMore()
//     input.head match {
//       case node: Node =>
//         if (input.size > 1) PartialMatch(SingleMatch(node, 0))
//         else FullMatch(SingleMatch(node, 0), couldMatchMore = false)
//       case _ => NoMatch()
//     }
//   }
// }
