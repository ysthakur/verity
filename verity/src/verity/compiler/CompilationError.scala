package verity.compiler

import verity.compiler.ast.Span

class CompilationError(val span: Span, val msg: String)
