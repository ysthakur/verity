package verity.compiler.resolve

import scala.collection.{mutable => mut}

import cats.data.Chain
import cats.data.NonEmptyList

/** A node in a directed graph
  *
  * @param to
  *   The nodes that this node has connections to
  * @see
  *   Graph
  */
case class Node[T](values: NonEmptyList[T], deps: Seq[Node[T]])

/** A directed graph. Can actually be multiple unconnected graphs. Although the
  * original graph can be cyclic, if there are any cycles, they are all merged
  * into one single node
  */
case class Graph[T](val nodes: Seq[Node[T]])

object Graph {

  /** Turn a Map giving the dependencies of each node into a proper [[Graph]] */
  def from[T](toDeps: Map[T, Iterable[T]]): Graph[T] = {
    // Maps each node value to the connected component it's in
    val components = mut.Map.empty[T, Seq[T]]

    /** Add a node and all its dependents to `components`, depth-first
      *
      * @param prev
      *   The list of previously seen nodes, in reverse
      */
    def makeComponents(value: T, prev: List[T]): Unit = {
      if (!components.contains(value)) {
        val ind = prev.indexOf(value)
        val newPrev = value :: prev
        if (ind == -1) {
          // No cycles yet
          components(value) = Seq(value)
          toDeps(value).foreach { dep => makeComponents(dep, newPrev) }
        } else {
          val cycle = prev.take(ind + 1)
          val newComponent = cycle.flatMap { node =>
            components.getOrElse(node, Seq(node))
          }
          newComponent.foreach { node =>
            components(node) = newComponent
          }
          for {
            node <- newComponent
            dep <- toDeps(node)
          } {
            makeComponents(dep, newPrev)
          }
        }
      }
    }

    // Build up components
    toDeps.keys.foreach(makeComponents(_, List.empty))

    val added = mut.Set.empty[T]
    val nodes = mut.ListBuffer.empty[Node[T]]

    def addNode(value: T)

    components.foreach { (value, component) =>
      if (!added(value)) {

      }
    }

    ???
  }
}
