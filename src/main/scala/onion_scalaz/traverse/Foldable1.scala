package onion_scalaz.traverse

import onion_scalaz.monoid.Semigroup

trait Foldable1[F[_]] extends Foldable[F] {
  self =>


  def foldMap1[A, B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B


  def foldMap1Opt[A, B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] =
    Some(foldMap1(fa)(f))


  def foldMapRight1[A, B](fa: F[A])(z: A => B)(f: (A, => B) => B): B


}
