package onion_scalaz.morphism

import onion_scalaz.monoid.Monoid
import onion_scalaz.traverse.{Foldable, Foldable1}

trait Product[F[_], G[_]] extends Functor[λ[a => (F[a], G[a])]] {

  implicit def F: Functor[F]

  implicit def G: Functor[G]


  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) =
    (F.lift(f)(fa._1), G.lift(f)(fa._2))
}


trait ProductFoldable[F[_], G[_]] extends Foldable[λ[α => (F[α], G[α])]] {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: (F[A], G[A]), z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa._1, G.foldRight(fa._2, z)(f))(f)

  override def foldMap[A, B](fa: (F[A], G[A]))(f: A => B)(implicit M: Monoid[B]): B =
    M.append(F.foldMap(fa._1)(f), G.foldMap(fa._2)(f))

  override def foldLeft[A, B](fa: (F[A], G[A]), z: B)(f: (B, A) => B): B =
    G.foldLeft(fa._2, F.foldLeft(fa._1, z)(f))(f)
}

trait ProductFoldable1[F[_], G[_]] extends Foldable1[λ[α => (F[α], G[α])]] with ProductFoldable[F, G] {

}

