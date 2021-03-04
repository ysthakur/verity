package verity.ast.infile

import verity.ast.infile.Expr

/**
  * A variable declaration (local variable or field)
  */
trait IVariableDecl extends Node with HasType {
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
