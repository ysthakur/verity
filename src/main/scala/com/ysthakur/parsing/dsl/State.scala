package com.ysthakur.parsing.dsl

case class State(name: String) {
    def :=[I](patternCases: PatternCase[I]*)
                            (implicit tokenizer: Tokenizer[I, _, _]): Unit =
        tokenizer.addStateCase(StateCase(this, patternCases))
}