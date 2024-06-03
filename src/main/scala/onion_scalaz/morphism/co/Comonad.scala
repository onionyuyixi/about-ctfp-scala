package onion_scalaz.morphism.co

import onion_scalaz.<~>
import scalaz.Equal

trait Comonad[F[_]] extends Cobind[F] {


  def copoint[A](fa: F[A]): A


  trait ComonadLaws extends CobindLaws {
    def cobindLeftIdentity[A](fa: F[A])(implicit F: Equal[F[A]]): Boolean =
      F.equal(cobind(fa)(copoint), fa)

    def cobindRightIdentity[A, B](fa: F[A], f: F[A] => B)(implicit F: Equal[B]): Boolean =
      F.equal(copoint(cobind(fa)(f)), f(fa))
  }

  def comonadLaw: ComonadLaws = new ComonadLaws {}


}

object Cobind {

  @inline def apply[F[_]](implicit F: Comonad[F]): Cobind[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Comonad[G]): Cobind[F] =
    new IsomorphismComonad[F, G] {
      override implicit def G: Comonad[G] = E

      override def iso: F <~> G = D
    }

}

trait IsomorphismComonad[F[_], G[_]] extends Comonad[F] with IsomorphismCobind[F, G] {

  implicit def G: Comonad[G]

  override def copoint[A](fa: F[A]): A =
    G.copoint(iso.to(fa))
}
