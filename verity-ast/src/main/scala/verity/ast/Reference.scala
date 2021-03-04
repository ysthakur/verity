package verity.ast

trait Reference[+T <: Tree] {
  def resolve: Option[T]
}