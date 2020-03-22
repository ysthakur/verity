package com.ysthakur.parsing

/**
 * Represents a range of text (or anything else, really).
 * @param start The start of this range (inclusive)
 * @param end The end of this range (exclusive)
 */
case class TextRange(start: Int, end: Int)
