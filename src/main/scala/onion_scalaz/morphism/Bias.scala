package onion_scalaz.morphism


trait LeftFunctor[F[_, _], X] extends Functor[F[*, X]] {

  implicit def F: BiFunctor[F]

  override def map[A, B](fa: F[A, X])(f: A => B): F[B, X] =
    F.bimap(fa)(f, identity[X])

}


trait RightFunctor[F[_, _], X] extends Functor[F[X, *]] {

  implicit def F: BiFunctor[F]

  override def map[A, B](fa: F[X, A])(f: A => B): F[X, B] =
    F.bimap(fa)(identity[X], f)

}
