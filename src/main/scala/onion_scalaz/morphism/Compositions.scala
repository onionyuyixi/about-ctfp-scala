package onion_scalaz.morphism

import scalaz.Bifunctor


trait FunctorComposition[F[_], G[_]] extends Functor[λ[x => F[G[x]]]] {

  implicit def F: Functor[F]

  implicit def G: Functor[G]

  // compose 的意思就是 先计算在Functor中 先G 在F
  def map[A, B](fga: F[G[A]])(f: A => B) =
    F(fga)(G.lift(f))


}

trait BiFunctorComposition[F[_], G[_, _]] extends BiFunctor[λ[(α, β) => F[G[α, β]]]] {

  implicit def F: Functor[F]

  implicit def G: BiFunctor[G]

  def map[A, B, C, D](gab: G[A, B]): F[G[C, D]] = ???




}
