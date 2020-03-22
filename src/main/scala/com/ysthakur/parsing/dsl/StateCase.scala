package com.ysthakur.parsing.dsl

case class StateCase[Input](state: State, patternCases: Iterable[PatternCase[Input]])
