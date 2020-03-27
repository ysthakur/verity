package com.ysthakur

class CompilationError(msg: String = "", cause: Throwable = null)
    extends Exception(s"Compilation error: $msg", cause) {
    def unapply(arg: CompilationError): Option[(String, Throwable)] = Some((msg, cause))
}
