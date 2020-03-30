package com.ysthakur.parsing.ast

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.grammar.SingleMatch
import com.ysthakur.parsing.lexer.InvariantToken

package object infile {

  implicit val ValidIdMaker: SingleMatch[Node] => ValidIdNode =
    m => ValidIdNode(m.matched.asInstanceOf[InvariantToken].text)
}
