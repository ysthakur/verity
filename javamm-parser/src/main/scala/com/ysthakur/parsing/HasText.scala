package com.ysthakur.parsing

trait HasText {
  def text: CharSequence
  def startOffset: Int
  def endOffset: Int
}
