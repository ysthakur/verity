package com.ysthakur.verity.parsing.ast

case class OptionNode[T <: Node](opt: Option[T]) extends Node {
  def text = opt.map(_.text).getOrElse("")
  def textRange = opt.map(_.textRange).get
}