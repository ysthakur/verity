package com.ysthakur.verity.parsing.ast.infile

<<<<<<< HEAD
import com.ysthakur.verity.parsing.{HasText, TextRange}
=======
import com.ysthakur.verity.parsing.HasText
>>>>>>> master
import com.ysthakur.verity.parsing.ast.INode

trait Node extends INode with HasText {
  override def flatten: Node = this
  def text: String
<<<<<<< HEAD
  def textRange: TextRange
}
=======
}
>>>>>>> master
