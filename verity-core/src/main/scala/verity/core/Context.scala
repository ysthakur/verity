package verity.core

import verity.ast.*
import verity.ast.infile.*

import Context.Refs

type GivenOrProof = Expr | VariableDecl | Methodlike

case class Context(
    varRefs: Refs[VariableDecl],
    mthdRefs: Refs[MethodGroup],
    givens: Iterable[GivenOrProof],
    proofs: Iterable[GivenOrProof],
    clsRefs: Refs[Classlike],
    pkgRefs: Refs[Package],
    cls: Classlike,
    file: FileNode
)

object Context {
  type Refs[T] = Map[String, T] // & Iterable[Ref[T]]
  // opaque type Ref[T] <: (String, T) = (String, T)
  
  // extension [T](ref: Ref[T])
  //   def name: String = ref._1
  //   def decl: T = ref._2
}