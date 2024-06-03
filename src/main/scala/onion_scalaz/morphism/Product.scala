package onion_scalaz.morphism

import onion_scalaz.monoid.Monoid
import onion_scalaz.traverse.{Foldable, Foldable1}

trait ProductFunctor[F[_], G[_]] extends Functor[λ[a => (F[a], G[a])]] {

  implicit def F: Functor[F]

  implicit def G: Functor[G]


  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) =
    (F.lift(f)(fa._1), G.lift(f)(fa._2))
}

trait ProductApply[F[_], G[_]] extends Apply[λ[α => (F[α], G[α])]] with ProductFunctor[F, G] {

  implicit def F: Apply[F]

  implicit def G: Apply[G]

  override def ap[A, B](fa: => (F[A], G[A]))(f: => (F[A => B], G[A => B])): (F[B], G[B]) =
    (F.ap(fa._1)(f._1), G.ap(fa._2)(f._2))

}

trait ProductApplicative[F[_], G[_]] extends Applicative[λ[α => (F[α], G[α])]] with ProductApply[F, G] {

  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  override def point[A](a: => A): (F[A], G[A]) = (F.point(a), G.point(a))
}

trait ProductBind[F[_], G[_]] extends Bind[λ[α => (F[α], G[α])]] with ProductApply[F, G] {
  implicit def F: Bind[F]

  implicit def G: Bind[G]

  override def bind[A, B](fa: (F[A], G[A]))(f: A => (F[B], G[B])) =
    (F.bind(fa._1)(f.andThen(_._1)), G.bind(fa._2)(f.andThen(_._2)))
}

trait ProductMonad[F[_], G[_]] extends Monad[λ[α => (F[α], G[α])]] with ProductBind[F, G]
  with ProductApplicative[F, G] {

  implicit def F: Monad[F]

  implicit def G: Monad[G]

}


private trait ProductBifunctor[F[_, _], G[_, _]] extends BiFunctor[λ[(α, β) => (F[α, β], G[α, β])]] {

  implicit def F: BiFunctor[F]

  implicit def G: BiFunctor[G]

  override def bimap[A, B, C, D](fab: (F[A, B], G[A, B]))(f: A => C, g: B => D): (F[C, D], G[C, D]) =
    (F.bimap(fab._1)(f, g), G.bimap(fab._2)(f, g))


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


