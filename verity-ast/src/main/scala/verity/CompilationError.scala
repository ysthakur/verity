package verity

class CompilationError(msg: String = "", cause: Throwable|Null = null)
    extends Exception(s"Compilation error: $msg", cause) {
  def unapply(arg: CompilationError): Option[(String, Throwable|Null)] =
    Some((msg, cause))
}