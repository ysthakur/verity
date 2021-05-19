package verity.core

import verity.ast._
import verity.ast.infile._
import verity.core.Context.Defs

type GivenDef = VariableDecl | Methodlike
//type ProofDef = VariableDecl | Methodlike | Expr

case class Context(
    varDefs: Defs[VariableDecl],
    mthdDefs: Defs[MethodGroup],
    givenDefs: Iterable[GivenDef],
    proofDefs: Iterable[GivenDef],
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg],
    cls: Classlike,
    file: FileNode
)

object Context {
  type Defs[T] = Map[String, T]
}
