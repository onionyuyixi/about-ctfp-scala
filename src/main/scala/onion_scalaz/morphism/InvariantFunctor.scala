package onion_scalaz.morphism

import onion_scalaz.{<=>, <~>, Bijection, IsoSet, NaturalTrans}
import scalaz.Alpha.B

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