package onion_scalaz.morphism

import onion_scalaz._
import scalaz.Equal

// 不变的Functor xmap 有两个函数
// Also known as an exponential functor.
trait InvariantFunctor[F[_]] {

  self =>

  def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]

  def xmapi[A, B](ma: F[A])(iso: A <=> B): F[B] = xmap(ma, iso.to, iso.from)

  def xmapb[A, B](ma: F[A])(b: Bijection[A, B]): F[B] = {
    xmap(ma, b.to, b.from)

    xmapi(ma)(new IsoSet[A, B] {
      override def to: A => B = b._to
      override def from: B => A = b._from
    })
  }

  trait InvariantFunctorLaw {

    def invariantIdentity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(xmap[A, A](fa, x => x, x => x), fa)

    def invariantComposite[A, B, C](fa: F[A], f1: A => B, g1: B => A, f2: B => C, g2: C => B)(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(xmap(xmap(fa, f1, g1), f2, g2), xmap(fa, f2 compose f1, g1 compose g2))
  }


}


object InvariantFunctor {

  @inline def apply[F[_]](implicit fa: InvariantFunctor[F]): InvariantFunctor[F] = fa

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: InvariantFunctor[G]): InvariantFunctor[F] =
    new IsomorphismInvariantFunctor[F, G] {
      override implicit def G: InvariantFunctor[G] = E

      override def iso: F <~> G = D
    }

}


trait IsomorphismInvariantFunctor[F[_], G[_]] extends InvariantFunctor[F] {


  implicit def G: InvariantFunctor[G]

  def iso: F <~> G


  override def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B] = {

    val to: NaturalTrans[F, G] = iso.to
    val from: NaturalTrans[G, F] = iso.from
    from(G.xmap(to(ma), f, g))

  }

}