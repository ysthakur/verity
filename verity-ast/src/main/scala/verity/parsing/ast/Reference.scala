package verity.parsing.ast

trait Reference[+T <: INode] {
  def resolve: Option[T]
}