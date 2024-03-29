package verity.compiler

import verity.compiler.ast.ModuleDef
import verity.compiler.ast.TypeDef
import verity.compiler.ast.VarDef

/** Information about the current context/scope.
  *
  * Construct using [[Context.apply]]
  *
  * @param parent
  *   The context outside this one, None if this is the root context
  * @param moduleDefs
  *   Maps the names of the modules in this context to their definitions
  *   themselves
  * @param typeDefs
  *   Maps the names of the TypeDefs in this context to their definitions
  *   themselves
  * @param varDefs
  *   Maps the names of the VarDefs in this context to their definitions
  *   themselves
  * @param File
  *   The file we're currently in
  */
class Context private (
    val file: Source,
    val parent: Option[Context],
    val moduleDefs: Map[String, ModuleDef],
    val typeDefs: Map[String, TypeDef],
    val varDefs: Map[String, VarDef],
) {

  /** Make a child context using the current context */
  def child(
      moduleDefs: Iterable[ModuleDef] = Nil,
      typeDefs: Iterable[TypeDef] = Nil,
      varDefs: Iterable[VarDef] = Nil,
  ): Context =
    Context(this.file, Some(this), moduleDefs, typeDefs, varDefs)
}

object Context {

  /** Make a Context
    *
    * @param file
    *   The file we're currently in
    * @param parent
    *   The context outside this one, None if this is the root context
    * @param moduleDefs
    *   The modules in the context.
    * @param typeDefs
    *   The types in this context.
    * @param varDefs
    *   The variables in this context.
    */
  def apply(
      file: Source,
      parent: Option[Context],
      moduleDefs: Iterable[ModuleDef] = Nil,
      typeDefs: Iterable[TypeDef] = Nil,
      varDefs: Iterable[VarDef] = Nil,
  ) = new Context(
    file,
    parent,
    moduleDefs.view.map(mod => mod.name -> mod).toMap,
    typeDefs.view.map(typeDef => typeDef.name -> typeDef).toMap,
    varDefs.view.map(varDef => varDef.name -> varDef).toMap,
  )

  /** Execute inside a child context of the current context */
  inline def inChild[T](body: Context ?=> T)(using outerCtx: Context): T =
    body(using outerCtx.child())
}
