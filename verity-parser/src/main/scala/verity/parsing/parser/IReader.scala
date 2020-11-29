package verity.parsing.parser

import collection.mutable.ArrayBuffer

import verity.parsing._

case class IReader(
    offset: Int,
    charInd: Int,
    chars: StringBuilder,
    tokens: ArrayBuffer[Token],
    comments: ArrayBuffer[Token]
) {}
