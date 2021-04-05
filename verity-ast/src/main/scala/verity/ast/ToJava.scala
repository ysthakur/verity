package verity.ast

import verity.parsing.*
import verity.ast.infile.*

import scala.deriving.*
import scala.compiletime.{summonAll as _, *}
import scala.quoted.*

trait ToJava[T] {
  def verityToJava(tree: T): String
  extension (tree: T)
    inline def toJava: String = verityToJava(tree)
}

object ToJava {
  def apply[T](using tj: ToJava[T]) = tj

  given ToJava[Name] = name => name.toString

  given ToJava[Iterable[Name]] = seq => seq.view.map(_.toJava).mkString(".")

  // given ToJava[DotRef] = dotRef => dotRef.path.view.map(_.toString).mkString(".")

  inline given derived[T](using m: Mirror.Of[T]): ToJava[T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => toJavaSum(s, elemInstances.asInstanceOf[List[ToJava[_]]])
      case p: Mirror.ProductOf[T] => toJavaProduct(p, elemInstances.asInstanceOf[List[ToJava[_]]])

  inline def summonAll[T <: Tuple]: List[ToJava[_]] =
    inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts) => summonInline[ToJava[t]] :: summonAll[ts]

  def toJavaSum[T](s: Mirror.SumOf[T], instances: => List[ToJava[_]]): ToJava[T] =
    new ToJava[T]:
      def verityToJava(tree: T): String =
        val ord = s.ordinal(tree)
        convertToJava(tree, instances(ord))

   def toJavaProduct[T](p: Mirror.ProductOf[T], instances: => List[ToJava[_]]): ToJava[T] =
    new ToJava[T]:
      def verityToJava(tree: T): String =
        p.asInstanceOf[Product].productIterator.zip(instances).map {
          (child, tj) => convertToJava(child, tj)
        }.mkString(" ")
  
  private def convertToJava(tree: Any, tj: ToJava[_]) = tj.asInstanceOf[ToJava[Any]].toJava(tree) 
}