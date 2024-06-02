package onion_scalaz.traverse

import onion_scalaz.monoid.{Monoid, Semigroup}
import onion_scalaz.morphism.{CompositionFoldable, ProductFoldable}

trait Foldable[F[_]] {
  self =>

  // 这是体现核心的方法 Monoid为其提供zero append
  // 使得计算结果 可以累计起来
  // 两端计算结果都一致
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  def foldMap1Opt[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): Option[B] =
    Option(foldMap(fa)(x => f(x)))

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B


  def compose[G[_]](implicit G0: Foldable[G]): Foldable[λ[α => F[G[α]]]] =
    new CompositionFoldable[F, G] {
      override implicit def F: Foldable[F] = self

      override implicit def G: Foldable[G] = G0
    }

  def product[G[_]](implicit G0: Foldable[G]): Foldable[λ[a => (F[a], G[a])]] =
    new ProductFoldable[F, G] {
      override implicit def F: Foldable[F] = self

      override implicit def G: Foldable[G] = G0
    }


}


object Foldable {

  @inline def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F


}