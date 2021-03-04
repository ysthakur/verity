package verity.ast

trait Reference[+T <: INode] {
  def resolve: Option[T]
}