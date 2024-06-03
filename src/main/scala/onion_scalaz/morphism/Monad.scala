package onion_scalaz.morphism

import onion_scalaz.<~>
import scalaz.Equal


// 结合了 Applicative 的lift and point  Bind的跨容器绑定
trait Monad[F[_]] extends Applicative[F] with Bind[F] {


  self =>


  // 这里主要阐释了 lift的作用就是 通过point 和 bind 来实现的
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    bind(fa)(a => point(f(a)))


  def product[G[_]](implicit G0: Monad[G]): Monad[λ[α => (F[α], G[α])]] =
    new ProductMonad[F, G] {
      def F = self

      def G = G0
    }

  trait MonadLaw extends ApplicativeLaw with BindLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)

    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
  }


}


object Bind {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Monad[G]): Bind[F] =
    new IsomorphismMonad[F, G] {
      override def G: Monad[G] = E

      override def iso: F <~> G = D
    }

  ////


  ////
}

trait IsomorphismMonad[F[_], G[_]] extends Monad[F] with IsomorphismApplicative[F, G] with IsomorphismBind[F, G] {
  override implicit def G: Monad[G]
  ////

  ////
}