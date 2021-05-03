package verity.core

import verity.ast.*
import verity.ast.infile.*

import Context.Refs

case class Context(
    varRefs: Refs[VariableDecl],
    mthdRefs: Refs[MethodGroup],
    givens: List[Expr | Methodlike],
    proofs: List[Expr | Methodlike],
    clsRefs: Refs[Classlike],
    pkgRefs: Refs[Package],
    cls: Classlike,
    file: FileNode
)

object Context {
  type Refs[T] = Map[String, T]
}