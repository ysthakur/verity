package com.ysthakur.parsing.ast

import com.ysthakur.parsing.ast
import com.ysthakur.parsing.ast.infile

object Types {
  type Node = ast.Node
  type FileNode = ast.FileNode
  type PackageNode = ast.PackageNode

  type Expr = infile.expr.Expr
}
