package onion_scalaz.monoid

import onion_scalaz.<~>
import onion_scalaz.morphism.functors.ProductPlusEmpty
import onion_scalaz.morphism.{Applicative, PlusEmptyComposition}
import scalaz.Equal


// 更广泛版本的Monoid
trait PlusEmpty[F[_]] extends Plus[F] {

  self =>

  def empty[A]: F[A]

  override def compose[G[_]]: PlusEmpty[λ[α => F[G[α]]]] =
    new PlusEmptyComposition[F, G] {

      override implicit def F: PlusEmpty[F] = self
    }

  def product[G[_]](implicit G0: PlusEmpty[G]): PlusEmpty[λ[α => (F[α], G[α])]] =
    new ProductPlusEmpty[F, G] {
      override def F: PlusEmpty[F] = self

      override def G: PlusEmpty[G] = G0
    }

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      override def zero: F[A] = self.empty

      override def append(f1: F[A], f2: => F[A]): F[A] = self.plus(f1, f2)
    }

  trait EmptyLaw extends PlusLaw {
    def rightPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, empty[A]), f1)

    def leftPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(empty[A], f1), f1)
  }

  def plusEmptyLaw: EmptyLaw =
    new EmptyLaw {}


}

object PlusEmpty {

  @inline def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: PlusEmpty[G]): PlusEmpty[F] =
    new IsomorphismPlusEmpty[F, G] {
      override def G: PlusEmpty[G] = E

      override def iso: F <~> G = D
    }

  implicit def liftPlusEmpty[M[_], N[_]](implicit M: Applicative[M], P: PlusEmpty[N]): PlusEmpty[λ[α => M[N[α]]]] =
    new Plus.LiftedPlus[M, N] with PlusEmpty[λ[α => M[N[α]]]] {
      def G: Applicative[M] = M

      def F: PlusEmpty[N] = P

      def empty[A]: M[N[A]] = M.point(P.empty[A])
    }
}

trait IsomorphismPlusEmpty[F[_], G[_]] extends PlusEmpty[F] with IsomorphismPlus[F, G] {

  implicit def G: PlusEmpty[G]

  def empty[A]: F[A] = iso.from(G.empty[A])
}