package com.ysthakur.parsing

trait HasText {
  def text: String
  def startOffset: Int
  def endOffset: Int
}
