package verity.ast.infile

import verity.ast._

case class Annotation(appliedTo: INode)

trait HasAnnotations {
  def annotations: Iterable[Annotation]
}