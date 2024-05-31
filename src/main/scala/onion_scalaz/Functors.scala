
package onion_scalaz


trait Functor[F[_]] {

  def map[A, B](fa: F[A]): F[B]


}


trait BiFunctor[F[_, _]] {

  def map[A, B, C, D](fa: F[A, B]): F[C, D]


}


object Functors {

}
