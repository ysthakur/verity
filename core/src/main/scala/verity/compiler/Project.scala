import cats.data.NonEmptyList

/** Represents the entire project that's being compiled
  * @param root
  *   The root package
  */
class Project {
  val root = VerityPkg("__root__")

  /** Find a package by its absolute path. If the package doesn't exist, creates
    * one.
    */
  def getPackage(path: NonEmptyList[String]): VerityPkg =
    root.getPackageRelative(path)
}
