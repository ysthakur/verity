package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

/**
  * A variable declaration (local variable or field)
  */
trait IVariableDecl extends Node {
  /**
    * The type of this variable, or `None` if the type is inferred
    * @return
    */
  def varType: Option[TypeRef]
  def name: String
  /**
    * What it gets initialized to, unless it's just declared
    * @return
    */
  def initExpr: Option[Expr]

  /**
    * Whether or not this is simply a declaration
    * @return True if only a declaration, false if also intialized
    */
  def declarationOnly: Boolean = initExpr == None
}
