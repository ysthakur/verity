package com.ysthakur.verity.parsing.ast.infile

import scala.collection.mutable.ListBuffer

trait CTParam extends Node


case class CTParamList(ctparams: ListBuffer[CTParam]) extends Node {
  override def text: String = s"<${ctparams.mkString(",")}>"
}