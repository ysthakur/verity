package com.ysthakur

import com.ysthakur.parsing.lexer.RegexPattern

package object parsing {

    implicit def toState[T](stateName: String)(implicit states: Map[String, State]): State =
        states.getOrElse(stateName, throw new NullPointerException())

    implicit def toRegex(regex: String): RegexPattern = RegexPattern(regex)

}
