package com.ysthakur.parsing.dsl

case class State(name: String) {
    def :=[I](patternCases: PatternCase[I]*)
                            (implicit tokenizer: LexerOrParser[I, _, _]): Unit =
        tokenizer.addStateCase(StateCase(this, patternCases))
}