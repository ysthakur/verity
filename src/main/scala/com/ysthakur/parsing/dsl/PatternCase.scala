package com.ysthakur.parsing.dsl

case class PatternCase[+Input](pattern: Pattern[Input], action: () => Unit) {

}
