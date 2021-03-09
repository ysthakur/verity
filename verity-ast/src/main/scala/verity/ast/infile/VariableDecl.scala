package verity.ast.infile

/**
  * A variable declaration (local variable or field)
  */
trait VariableDecl extends Node, HasType, NamedTree {
  def name: Name
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
