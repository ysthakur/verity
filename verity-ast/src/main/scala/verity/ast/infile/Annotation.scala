package verity.ast.infile

import verity.ast._

case class Annotation(appliedTo: Tree)

trait HasAnnotations {
  def annotations: Iterable[Annotation]
}