package verity.util

import scala.collection.IterableOps

extension [A, I[x] <: IterableOps[x, I, I[x]]](it: I[A | Null])
  inline def filterNotNull: I[A] = it.filter(_ != null).asInstanceOf[I[A]]
  inline def removeNull: I[A] = it.asInstanceOf[I[A]]

extension [T](obj: T | Null)
  inline def unsafeNN: T = obj.asInstanceOf[T]

//Right associative, so arguments reversed
extension [T](orElse: T)
  inline def ?:(get: T | Null): T =
    if (get == null) orElse else get
