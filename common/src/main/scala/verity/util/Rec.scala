enum Rec[A] {
  case Done(value: A)
  case Suspend(get: () => A)
  case FlatMap[A, B](prev: Rec[B], next: B => Rec[A]) extends Rec[A]
}
