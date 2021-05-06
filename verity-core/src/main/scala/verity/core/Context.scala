package verity.core

import verity.ast.*
import verity.ast.infile.*
import verity.core.Context.Defs

type GivenOrProof = Expr | VariableDecl | Methodlike

case class Context(
    varDefs: Defs[VariableDecl],
    mthdDefs: Defs[MethodGroup],
    givenDefs: Iterable[GivenOrProof],
    proofDefs: Iterable[GivenOrProof],
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg],
    cls: Classlike,
    file: FileNode
)

object Context {
  type Defs[T] = Map[String, T] // & Iterable[Ref[T]]
  // opaque type Ref[T] <: (String, T) = (String, T)

  // extension [T](ref: Ref[T])
  //   def name: String = ref._1
  //   def decl: T = ref._2
}
