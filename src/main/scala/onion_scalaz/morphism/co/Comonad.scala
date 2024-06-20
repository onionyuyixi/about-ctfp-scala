package onion_scalaz.morphism.co

import onion_scalaz.<~>
import scalaz.Equal

import scala.annotation.tailrec

trait Comonad[F[_]] extends Cobind[F] {


  def copoint[A](fa: F[A]): A

  final def copure[A](fa: F[A]): A = copoint(fa)


  trait ComonadLaws extends CobindLaws {
    def cobindLeftIdentity[A](fa: F[A])(implicit F: Equal[F[A]]): Boolean =
      F.equal(cobind(fa)(copoint), fa)

    def cobindRightIdentity[A, B](fa: F[A], f: F[A] => B)(implicit F: Equal[B]): Boolean =
      F.equal(copoint(cobind(fa)(f)), f(fa))
  }

  def comonadLaw: ComonadLaws = new ComonadLaws {}


}

case class Stream[T](h: () => T, t: () => Stream[T])

object Comonad {

  @inline def apply[F[_]](implicit F: Comonad[F]): Cobind[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Comonad[G]): Cobind[F] =
    new IsomorphismComonad[F, G] {
      override implicit def G: Comonad[G] = E

      override def iso: F <~> G = D
    }

  implicit def streamComonad: Comonad[Stream] = new Comonad[Stream] {


    @tailrec
    override def copoint[A](fa: Stream[A]): A =
      fa match {
        case Stream(h, t) => Option(h) match {
          case Some(value) => value()
          case None => copoint(t())
        }
      }

    override def cobind[A, B](fa: Stream[A])(f: Stream[A] => B): Stream[B] =
      map(cojoin(fa))(f)


    override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] =
      fa match {
        case Stream(h, t) =>
          Stream(() => f(h()), () => map(t())(f))
      }
  }
}

trait IsomorphismComonad[F[_], G[_]] extends Comonad[F] with IsomorphismCobind[F, G] {

  implicit def G: Comonad[G]

  override def copoint[A](fa: F[A]): A =
    G.copoint(iso.to(fa))
}
