package verity.core

import verity.ast.{TextRange, HasText}

type ResolveResult[T] = Either[ErrorMsg, T]

case class ErrorMsg(msg: String, textRangeOrTree: TextRange | HasText)
