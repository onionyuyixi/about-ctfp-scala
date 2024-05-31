package parttwo


trait Monoid[A] {
  def unit: A

  def append(a: A, b: A): A
}

trait FreeMonoid[F[_,_]] {

  def unit[A,B]: Monoid[F[A,B]]

  def append[A,B, C](mfa: Monoid[F[A,B]], mfb: Monoid[F[B,C]])(f:A=>B)(g:B=>C): Monoid[F[A,C]]
}



