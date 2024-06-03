

package onion_scalaz.category_base

import onion_scalaz.<~~>
import onion_scalaz.monoid.Semigroup
import onion_scalaz.morphism.BiNaturalTrans
import scalaz.Equal

// 参考 Endomorphic上的注解
trait Compose[=>:[_, _]] {

  self =>

  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C


  protected[this] trait ComposeSemigroup[A] extends Semigroup[A =>: A] {
    override def append(f1: A =>: A, f2: => A =>: A): A =>: A = self.compose(f1, f2)
  }

  def semigroup[A]: Semigroup[A =>: A] = new ComposeSemigroup[A] {}


  trait ComposeLaw {
    // compose 支持交换律
    def associative[A, B, C, D](ab: (A =>: B), bc: (B =>: C), cd: (C =>: D))
                               (implicit E: Equal[A =>: D]): Boolean = {
      val v1 = compose(cd, compose(bc, ab))
      val v2 = compose(compose(cd, bc), ab)
      E.equal(v1, v2)
    }

  }


  object Compose {

    @inline def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F

    def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Compose[G]): Compose[F] = {
      val from: BiNaturalTrans[G, F] = D.from
      val to: BiNaturalTrans[F, G] = D.to
      new Compose[F] {
        override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = {
          val gac: G[A, C] = E.compose(to(f), to(g))
          from(gac)
        }
      }
    }


    def fromIso1[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Compose[G]): Compose[F] = new IsomorphismCompose[F, G] {
      override implicit def G: Compose[G] = E

      override def iso: F <~~> G = D
    }
  }

  // G F 之间的互相映射
  trait IsomorphismCompose[F[_, _], G[_, _]] extends Compose[F] {

    implicit def G: Compose[G]

    def iso: F <~~> G

    override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
      iso.from(G.compose(iso.to(f), iso.to(g)))
  }
}