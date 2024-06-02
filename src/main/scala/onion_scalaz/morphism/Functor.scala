
package onion_scalaz.morphism

import onion_scalaz.{<~<, <~>, NaturalTrans}


trait Functor[F[_]] extends InvariantFunctor[F] {

  self =>

  // lift 函数f 是functor 最基本的定义
  // lift map apply 三个方法 同根同源 可以互相直接转换
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def apply[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

  def map[A, B](fa: F[A])(f: A => B): F[B]


  def strengthL[A, B](a: A, fb: F[B]): F[(A, B)] = map(fb)((a, _))

  def strengthR[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)((_, b))

  def mapply[A, B](a: A)(fab: F[A => B]): F[B] = map(fab)(_(a))

  def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] =
    map(fa)(a => (a, f(a)))

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => Unit)

  def compose[G[_]](implicit G0: Functor[G]): Functor[λ[α => F[G[α]]]] = new FunctorComposition[F, G] {

    override implicit def F: Functor[F] = self

    override implicit def G: Functor[G] = G0
  }


  def bicompose[G[_, _]](implicit G0: BiFunctor[G]): BiFunctor[λ[(a, b) => F[G[a, b]]]] = ???

  def product[G[_]](implicit G0: Functor[G]): Functor[λ[a => (F[a], G[a])]] = ???

  // 通过liskov 替换扩大了 A,B covariant 的scope
  def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
    map(fa)(ev)


}


object Functor {

  @inline def apply[F[_]](implicit f: Functor[F]): Functor[F] = f

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Functor[G]): Functor[F] = new IsomorphismFunctor[F, G] {
    override implicit def G: Functor[G] = E

    override def iso: F <~> G = D
  }

}


trait IsomorphismFunctor[F[_], G[_]] extends Functor[F] with IsomorphismInvariantFunctor[F, G] {

  override implicit def G: Functor[G]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = iso.from(G.map(iso.to(fa))(f))
}














