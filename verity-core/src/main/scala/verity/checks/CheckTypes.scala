package verity.checks

import verity.ast.{TextRange, Type, TypeParam}
import verity.ast.infile.*
import verity.core.*

object CheckTypes {

  /** Check type arguments and return compiler messages where there are problems.
    */
  def checkTypeArgs(
    args: Iterable[Type],
    params: Iterable[TypeParam],
    typeArgsRange: TextRange
  ): List[CompilerMsg] = {
    val numParams = params.size
    val numArgs = args.size
    if (numParams == numArgs)
      args.lazyZip(params).flatMap(checkTypeArg).toList
    else
      List(
        errorMsg(
          s"Too ${if (numParams < numArgs) "many" else "few"} type arguments: expected $numParams, got $numArgs",
          typeArgsRange
        )
      )
  }

  /** Check a type argument, and if there are problems, return error messages.
    */
  private def checkTypeArg(args: Type, param: TypeParam): Iterable[CompilerMsg] =
    Nil //TODO!!!!!!!!!!!!1
}
