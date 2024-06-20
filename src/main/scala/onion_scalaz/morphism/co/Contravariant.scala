package onion_scalaz.morphism.co

import onion_scalaz.morphism.functors.{Functor, InvariantFunctor, IsomorphismInvariantFunctor}
import onion_scalaz.{<~<, <~>}
import scalaz.Equal

trait Contravariant[F[_]] extends InvariantFunctor[F] {

  self =>

  def contramap[A, B](fa: F[A])(f: B => A): F[B]


  def narrow[A, B](fa: F[A])(implicit ev: B <~< A): F[B] =
    contramap(fa)(ev)

  def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] =
    contramap(fa)(g)


  def compose[G[_]](implicit G0: Contravariant[G]): Functor[λ[α => F[G[α]]]] =
    new Functor[λ[α => F[G[α]]]] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
        self.contramap(fa)(gb => G0.contramap(gb)(f))
    }

  def icompose[G[_]](implicit G0: Functor[G]): Contravariant[λ[α => F[G[α]]]] =
    new Contravariant[λ[α => F[G[α]]]] {
      override def contramap[A, B](fa: F[G[A]])(f: B => A): F[G[B]] =
        self.contramap(fa)(gb => G0.map(gb)(f))
    }

  def product[G[_]](implicit G0: Contravariant[G]): Contravariant[λ[α => (F[α], G[α])]] =
    new Contravariant[λ[α => (F[α], G[α])]] {
      override def contramap[A, B](fa: (F[A], G[A]))(f: B => A): (F[B], G[B]) =
        (self.contramap(fa._1)(f), G0.contramap(fa._2)(f))
    }

  trait ContravariantLaw extends InvariantFunctorLaw {
    /** The identity function, lifted, is a no-op. */
    def identity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(contramap(fa)(x => x), fa)

    /**
     * A series of contramaps may be freely rewritten as a single
     * contramap on a composed function.
     */
    def composite[A, B, C](fa: F[A], f1: B => A, f2: C => B)(implicit FC: Equal[F[C]]): Boolean = FC.equal(contramap(contramap(fa)(f1))(f2), contramap(fa)(f1 compose f2))
  }

  def contravariantLaw: ContravariantLaw = new ContravariantLaw {}


}


object Contravariant {
  @inline def apply[F[_]](implicit F: Contravariant[F]): Contravariant[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Contravariant[G]): Contravariant[F] =
    new IsomorphismContravariant[F, G] {
      override def G: Contravariant[G] = E

      override def iso: F <~> G = D
    }

}

trait IsomorphismContravariant[F[_], G[_]] extends Contravariant[F] with IsomorphismInvariantFunctor[F, G] {
  implicit def G: Contravariant[G]
  ////

  def iso: F <~> G

  override def contramap[A, B](r: F[A])(f: B => A): F[B] =
    iso.from(G.contramap(iso.to(r))(f))
}