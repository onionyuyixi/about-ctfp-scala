package onion_scalaz.morphism

trait Product [F[_],G[_]] extends Functor[λ[a => (F[a], G[a])]] {

  implicit def F:Functor[F]

  implicit def G:Functor[G]


  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) =
    (F.lift(f)(fa._1),G.lift(f)(fa._2))
}


