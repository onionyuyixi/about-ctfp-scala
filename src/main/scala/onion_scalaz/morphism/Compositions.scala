package onion_scalaz.morphism

import onion_scalaz.category_base.Compose
import onion_scalaz.monoid.{Monoid, Plus, PlusEmpty}
import onion_scalaz.morphism.functionusage.Kleisli
import onion_scalaz.morphism.functors.{BiFunctor, Functor}
import onion_scalaz.traverse.{Foldable, Foldable1}


trait FunctorComposition[F[_], G[_]] extends Functor[λ[x => F[G[x]]]] {

  implicit def F: Functor[F]

  implicit def G: Functor[G]

  // compose 的意思就是 先计算在Functor中 先G 在F
  def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F(fga)(G.lift(f))

}

trait BiFunctorComposition[F[_, _], G[_, _]] extends BiFunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] {

  implicit def F: BiFunctor[F]

  implicit def G: BiFunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] =
    F.bimap(fab)((a: G[A, B]) => G.bimap(a)(f, g), (a: G[A, B]) => G.bimap(a)(f, g))

}


trait ApplyComposition[F[_], G[_]] extends Apply[λ[a => F[G[a]]]] with FunctorComposition[F, G] {

  implicit def F: Apply[F]

  implicit def G: Apply[G]

  override def ap[A, B](fga: => F[G[A]])(f: => F[G[A => B]]): F[G[B]] = {


    val func1: G[A => B] => G[A] => G[B] = (ff: G[A => B]) => (ga: G[A]) => G.ap(ga)(ff)

    val func3: F[G[A] => G[B]] = F.map(f)(func1)

    F.ap(fga)(func3)


  }
}


trait KleisliCompose[F[_]] extends Compose[Kleisli[F, *, *]] {

  implicit def F: Bind[F]

  // 可以观察出 这里compose 实际上是Bind[F[_]] 在其作用
  // 盖Bind的核心在 A=>F[B] 这正好跟Kleisli的寓意 相吻合
  def compose[A, B, C](bc: Kleisli[F, B, C], ab: Kleisli[F, A, B]): Kleisli[F, A, C] = ab >=> bc
}


trait ApplicativeComposition[F[_], G[_]] extends ApplyComposition[F, G] with Applicative[λ[a => F[G[a]]]] {


  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  override def point[A](a: => A): F[G[A]] = F.point(G.point(a))

}

trait PlusComposition[F[_], G[_]] extends Plus[λ[α => F[G[α]]]] {

  implicit def F: Plus[F]

  override def plus[A](a: F[G[A]], b: => F[G[A]]): F[G[A]] =
    F.plus(a, b)
}

trait PlusEmptyComposition[F[_], G[_]] extends PlusEmpty[λ[α => F[G[α]]]] with PlusComposition[F, G] {

  implicit def F: PlusEmpty[F]

  override def empty[A]: F[G[A]] = F.empty[G[A]]
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