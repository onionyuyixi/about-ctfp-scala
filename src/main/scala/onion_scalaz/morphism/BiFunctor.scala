package onion_scalaz.morphism

trait BiFunctor[F[_, _]] {

  def map[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

}