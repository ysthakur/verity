package com.ysthakur.parsing.ast

import com.ysthakur.parsing.ast
import com.ysthakur.parsing.ast.infile

object Types {
  type Node = ast.Node
  type NodeList = ast.NodeList
  type FileNode = ast.FileNode
  type PackageNode = ast.PackageNode

  type TextNode = infile.TextNode
  type ValidIdNode = infile.ValidIdNode
  
  type Expr = infile.expr.Expr
}
