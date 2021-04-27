package verity.core

import verity.ast.*
import verity.ast.infile.*

import Context.Refs

case class Context(
    pkgs: Refs[Package],
    clss: Refs[Classlike],
    mthds: Refs[MethodGroup],
    vars: List[Refs[VariableDecl]],
    givens: List[Expr | Methodlike],
    proofs: List[Expr | Methodlike]
)

object Context {
  type Refs[T] = Map[String, T]
}