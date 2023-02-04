package verity.compiler

/** The place where some code comes from */
enum Source {
  case File(file: java.io.File)
  case Str(string: String)
}
