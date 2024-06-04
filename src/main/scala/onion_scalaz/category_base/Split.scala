package onion_scalaz.category_base

import onion_scalaz.<~~>

trait Split[=>:[_, _]] extends Compose[=>:] {
  self =>

  // 支持product
  // 把F[A,B] x F[C,D] = F[(A,C),(B.D)]
  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A, C) =>: (B, D)

}

object Split {
  @inline def apply[F[_, _]](implicit F: Split[F]): Split[F] = F


  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Split[G]): Split[F] =
    new IsomorphismSplit[F, G] {
      override def G: Split[G] = E

      override def iso: F <~~> G = D
    }

}

trait IsomorphismSplit[F[_, _], G[_, _]] extends Split[F] with IsomorphismCompose[F, G] {

  implicit def G: Split[G]

  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    iso.from(G.split(iso.to(f), iso.to(g)))
}

