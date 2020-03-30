package com.ysthakur.parsing

trait HasText {
  def text: String
  def unapply(): (CharSequence, Int, Int)
}
