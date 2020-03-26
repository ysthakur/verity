package com.ysthakur.parsing

import scala.language.existentials

case class StateCase[Input, Helper](state: State, patternCases: Iterable[PatternCase[Input, Helper]])

case class State(name: String) {
    def :=:[LOP <: LexerOrParser[I, _, Iterable[I] with Cloneable] forSome {type I}](patternCases: PatternCase[LOP#I, LOP#Helper]*)
                (implicit tokenizer: LOP): Unit =
        tokenizer.addStateCase(StateCase(this, patternCases.asInstanceOf))
}