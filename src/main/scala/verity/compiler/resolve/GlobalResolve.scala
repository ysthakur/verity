package verity.compiler.resolve

import verity.compiler.ast.{ModuleDef, TypeDef, VarDef}
import verity.compiler.Context

object GlobalResolve {
  def resolveModule(mod: ModuleDef)(using outerCtx: Context): Unit = {
    given Context = Context(
      parent = Some(outerCtx),
      moduleDefs = mod.submodules,
      typeDefs = mod.typeDefs,
      varDefs = mod.varDefs
    )

    mod.submodules.foreach(resolveModule)
    mod.typeDefs.foreach(resolveTypeDef)
    mod.varDefs.foreach(resolveVarDef)
  }

  def resolveTypeDef(typeDef: TypeDef)(using outerCtx: Context) = {
    val innerTypeDefs = typeDef.comptimeParams.typeParams
    given Context = Context(
      parent = Some(outerCtx),
      typeDefs = innerTypeDefs,
      varDefs = typeDef.fields
    )
  }

  def resolveVarDef(varDef: VarDef)(using Context): Unit = {}
}
