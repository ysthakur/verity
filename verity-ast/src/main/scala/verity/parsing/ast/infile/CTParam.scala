package verity.parsing.ast.infile

import scala.collection.mutable.ListBuffer

import verity.parsing.TextRange

trait CTParam extends Node


case class CTParamList(ctparams: ListBuffer[CTParam], override val textRange: TextRange) extends Node {
  override def text: String = s"<${ctparams.mkString(",")}>"
}