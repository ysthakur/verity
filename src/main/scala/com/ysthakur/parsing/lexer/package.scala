package com.ysthakur.parsing

import java.lang.CharSequence

package object lexer {

    def toStr(chars: Iterable[Char]): String = chars match {
        case cs: CharSequence => chars.toString()
        case _ => chars.mkString
    }

}
