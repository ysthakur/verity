package verity.compiler.resolve

import verity.compiler.ast.*
import verity.compiler.Context

/** Resolve types and signatures */
object GlobalResolve {
  def resolveModule(mod: ModuleDef)(using outerCtx: Context): Unit = {
    given Context = outerCtx.child(
      moduleDefs = mod.submodules,
      typeDefs = mod.typeDefs,
      varDefs = mod.varDefs,
    )

    mod.submodules.foreach(resolveModule)
    mod.typeDefs.foreach(resolveTypeDef)
    mod.varDefs.foreach(resolveGlobalVar)
  }

  def resolveTypeDef(typeDef: TypeDef)(using outerCtx: Context) = {
    given Context = outerCtx.child(
      typeDefs = typeDef.comptimeParams.typeParams,
      varDefs = typeDef.comptimeParams.allConstParams,
    )
  }

  def resolveGlobalVar(varDef: GlobalVar)(using outerCtx: Context): Unit = {
    given Context = outerCtx.child()
  }
}
