package com.ysthakur.parsing.dsl

case class State(name: String) {
    def :=[I, O, A](patternCases: PatternCase[I]*)
                            (implicit tokenizer: Tokenizer[I, O, A]): Unit =
        tokenizer.addStateCase(StateCase(this, patternCases))
}