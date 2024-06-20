package onion_scalaz.morphism

import onion_scalaz.<~>
import onion_scalaz.morphism.functors.ProductApplicative
import scalaz.Equal

trait Applicative[F[_]] extends Apply[F] {

  self =>

  // 这也是lift的一种 为了区别可以当做是一个wrapper 作为基础起点
  // 从概念上说 类似于Monoid的zero
  def point[A](a: => A): F[A]

  final def pure[A](a: => A): F[A] = point(a)


  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(point(f))

  override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(point(f))


  def compose[G[_]](implicit G0: Applicative[G]): Applicative[λ[α => F[G[α]]]] =
    new ApplicativeComposition[F, G] {
      override implicit def F: Applicative[F] = self

      override implicit def G: Applicative[G] = G0
    }

  def product[G[_]](implicit G0: Applicative[G]): Applicative[λ[a => (F[a], G[a])]] =
    new ProductApplicative[F, G] {
      override implicit def F: Applicative[F] = self

      override implicit def G: Applicative[G] = G0
    }


  trait ApplicativeLaw extends ApplyLaw {
    /** `point(identity)` is a no-op. */
    def identityAp[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(ap(fa)(point((a: A) => a)), fa)

    /** `point` distributes over function applications. */
    def homomorphism[A, B](ab: A => B, a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(point(ab)), point(ab(a)))

    /** `point` is a left and right identity, F-wise. */
    def interchange[A, B](f: F[A => B], a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(f), ap(f)(point((f: A => B) => f(a))))

    /** `map` is like the one derived from `point` and `ap`. */
    def mapLikeDerived[A, B](f: A => B, fa: F[A])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(map(fa)(f), ap(fa)(point(f)))
  }


}


object Applicative {
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Applicative[G]): Applicative[F] =
    new IsomorphismApplicative[F, G] {
      override def G: Applicative[G] = E

      override def iso: F <~> G = D
    }

  ////
}

trait IsomorphismApplicative[F[_], G[_]] extends Applicative[F] with IsomorphismApply[F, G] {

  override implicit def G: Applicative[G]

  def point[A](a: => A): F[A] =
    iso.from(G.point(a))

}