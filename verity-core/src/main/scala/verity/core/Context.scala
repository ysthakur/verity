package verity.core

import verity.ast._

import verity.core.Context.Defs

type GivenDef = VarDef | Methodlike
type ProofDef = VarDef | Methodlike | Expr

case class Context(
  varDefs: Defs[VarDef],
  mthdDefs: Defs[MethodGroup],
  givenDefs: List[GivenDef],
  proofDefs: List[ProofDef],
  typeDefs: Defs[TypeDef],
  pkgDefs: Defs[Package],
  cls: Classlike,
  file: FileNode
)

object Context {
  type Defs[T] = Map[String, T]
}
