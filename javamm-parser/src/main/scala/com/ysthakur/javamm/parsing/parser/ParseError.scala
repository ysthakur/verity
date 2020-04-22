package com.ysthakur.javamm.parsing.parser

case class ParseError(expected: Iterable[Any], got: Any, pos: Any) 
    extends Error(s"Expected one of [${expected.mkString(",")}], got $got at ${pos}")