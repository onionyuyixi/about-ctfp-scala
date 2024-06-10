package onion_scalaz.morphism.co

import onion_scalaz.category_base.Compose
import onion_scalaz.morphism.{Functor, Monad}
import scalaz.Profunctor

// 意在 F[A]=>B
final case class Cokleisli[F[_], A, B](run: F[A] => B) {
  self =>

  def apply(fa: F[A]): B =
    run(fa)


  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(c => g(run(b.map(c)(f))))

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C, B] = Cokleisli(run compose f)

  def map[C](f: B => C): Cokleisli[F, A, C] = Cokleisli(f compose run)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def <<=(a: F[A])(implicit F: Cobind[F]): F[B] =
    F.cobind(a)(run)

  def =>=[C](c: Cokleisli[F, B, C])(implicit F: Cobind[F]): Cokleisli[F, A, C] =
    Cokleisli(fa => c run <<=(fa))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: Cobind[F]): Cokleisli[F, C, B] =
    c =>= this

  def =<=[C](c: Cokleisli[F, C, A])(implicit F: Cobind[F]): Cokleisli[F, C, B] =
    compose(c)

}

object Cokleisli extends CokleisliInstances {

}

private trait CokleisliCompose[F[_]] extends Compose[Cokleisli[F, *, *]] {
  implicit def F: Cobind[F]

  override def compose[A, B, C](f: Cokleisli[F, B, C], g: Cokleisli[F, A, B]): Cokleisli[F, A, C] = f compose g
}

private trait CokleisliProfunctor[F[_]] extends Profunctor[Cokleisli[F, *, *]] {

  implicit def F: Functor[F]

  override def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D): Cokleisli[F, C, D] =
    fab.dimap(f, g)

  override final def mapfst[A, B, C](fa: Cokleisli[F, A, B])(f: C => A): Cokleisli[F, C, B] =
    Cokleisli[F, C, B](fc => fa(F.map(fc)(f)))

  override final def mapsnd[A, B, C](fa: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
    fa map f
}

sealed abstract class CokleisliInstances0 {

  implicit def cokleisliCompose[F[_]](implicit F0: Cobind[F]): Compose[Cokleisli[F, *, *]] =
    new CokleisliCompose[F] {
      override def F = F0
    }

  implicit def cokleisliProfunctor[F[_] : Functor]: Profunctor[Cokleisli[F, *, *]] =
    new CokleisliProfunctor[F] {
      def F = implicitly
    }
}

sealed abstract class CokleisliInstances extends CokleisliInstances0 {
  implicit def cokleisliMonad[F[_], R]: Monad[Cokleisli[F, R, *]] & BindRec[Cokleisli[F, R, *]] =
    new CokleisliMonad[F, R] {}

  implicit def cokleisliArrow[F[_]](implicit F0: Comonad[F]): Arrow[Cokleisli[F, *, *]] & ProChoice[Cokleisli[F, *, *]] =
    new CokleisliArrow[F] {
      override def F = F0
    }
}