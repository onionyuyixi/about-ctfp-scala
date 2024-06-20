package onion_scalaz.morphism.co

import onion_scalaz.<~>
import onion_scalaz.morphism.functors.{Functor, IsomorphismFunctor}
import scalaz.Equal

trait Cobind[F[_]] extends Functor[F] {

  self=>

  // bind 是将元素A 与容器中F[B] 绑定成morphism
  // cobind 是将元素则是将容器F[A] 与 元素B 绑定
  // 即规定了向增添容器方向为bind 反之较少容器为cobind
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  def cojoin[A](fa:F[A]):F[F[A]]

  // cobind的别名
  final def extend[A, B](fa: F[A])(f: F[A] => B): F[B] = cobind(fa)(f)



  def cojoin[A](fa: F[A]): F[F[A]] = {
    cobind(fa)(a => map(a)(identity))
    cobind(fa)(a => a)
  }

  trait CobindLaws {
    def cobindAssociative[A, B, C, D](fa: F[A], f: F[A] => B, g: F[B] => C, h: F[C] => D)(implicit F: Equal[D]): Boolean = {
      implicit val C = self
      val d1 = ((Cokleisli(f) =>= Cokleisli(g)) =>= Cokleisli(h)) run fa
      val d2 = (Cokleisli(f) =>= (Cokleisli(g) =>= Cokleisli(h))) run fa
      F.equal(d1, d2)
    }
  }

  def cobindLaw: CobindLaws = new CobindLaws {}

}

object Cobind {
  @inline def apply[F[_]](implicit F: Cobind[F]): Cobind[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Cobind[G]): Cobind[F] =
    new IsomorphismCobind[F, G] {
      override def G: Cobind[G] = E
      override def iso: F <~> G = D
    }

}

trait IsomorphismCobind[F[_], G[_]] extends Cobind[F] with IsomorphismFunctor[F, G]{

  implicit def G: Cobind[G]

  override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] =
    iso.from(G.cobind(iso.to(fa))(f.compose(iso.from.apply)))

  override def cojoin[A](a: F[A]): F[F[A]] =
    iso.from(G.map(G.cojoin(iso.to(a)))(iso.from.apply))
}
