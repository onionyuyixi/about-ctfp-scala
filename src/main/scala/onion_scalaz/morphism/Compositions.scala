package onion_scalaz.morphism

import onion_scalaz.monoid.Monoid
import onion_scalaz.traverse.{Foldable, Foldable1}


trait FunctorComposition[F[_], G[_]] extends Functor[λ[x => F[G[x]]]] {

  implicit def F: Functor[F]

  implicit def G: Functor[G]

  // compose 的意思就是 先计算在Functor中 先G 在F
  def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F(fga)(G.lift(f))


}

trait BiFunctorComposition[F[_], G[_, _]] extends BiFunctor[λ[(α, β) => F[G[α, β]]]] {

  implicit def F: Functor[F]

  implicit def G: BiFunctor[G]

  def map[A, B, C, D](gab: G[A, B]): F[G[C, D]] = ???


}

trait CompositionFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]] {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: F[G[A]], z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa, z)((a, b) => G.foldRight(a, b)(f))

  override def foldMap[A, B](fa: F[G[A]])(f: A => B)(implicit M: Monoid[B]): B =
    F.foldMap(fa)(G.foldMap(_)(f))

  override def foldLeft[A, B](fa: F[G[A]], z: B)(f: (B, A) => B): B =
    F.foldLeft(fa, z)((b, a) => G.foldLeft(a, b)(f))
}

private trait CompositionFoldable1[F[_], G[_]] extends Foldable1[λ[α => F[G[α]]]] with CompositionFoldable[F, G] {

}