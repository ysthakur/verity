package verity.util

import collection.IterableOps

extension [A, I[x] <: IterableOps[x, I, I[x]]](it: I[A | UncheckedNull])
  inline def filterNotNull: I[A] = it.filter(_ != null).asInstanceOf[I[A]]
  inline def removeNull: I[A] = it.asInstanceOf[I[A]]

extension [T](obj: T | Null)
  inline def unsafeNN: T = obj.asInstanceOf[T]