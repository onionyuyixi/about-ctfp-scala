package onion_scalaz.category_base

import onion_scalaz.<~~>
import onion_scalaz.morphism.functors.{Functor, InvariantFunctor}
import scalaz.Equal

// Profunctors are covariant on the right and contravariant on the left.
// 抽象的表达了 Functor 的效用
trait Profunctor[=>:[_, _]] {

  self =>

  // Contramap on A.
  def mapfst[A, B, C](fab: A =>: B)(f: C => A): C =>: B

  // Functor map on `B`
  def mapsnd[A, B, C](fab: A =>: B)(f: B => C): A =>: C


  def dimap[A, B, C, D](fab: A =>: B)(f: C => A)(g: B => D): C =>: D =
    mapfst(mapsnd(fab)(g))(f)


  protected[this] trait SndCovariant[C] extends Functor[C =>: *] {
    override def map[A, B](fa: C =>: A)(f: A => B): C =>: B = mapsnd(fa)(f)
  }

  // 可以用Profunctor 表达Functor
  def covariantInstance[C]: Functor[=>:[C, *]] =
    new SndCovariant[C] {}

  def invariantFunctor: InvariantFunctor[λ[α => α =>: α]] = new InvariantFunctor[λ[α => α =>: α]] {
    override def xmap[A, B](ma: A =>: A, f: A => B, g: B => A): B =>: B =
      mapfst(mapsnd(ma)(f))(g)
  }


  trait ProfunctorLaw {
    def identity[A, B](gab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = E.equal(dimap(gab)((a: A) => a)((b: B) => b), gab)

    def composite[A, B, C, D, E, F](gad: (A =>: D), fac: C => B, fba: B => A, fde: D => E, fef: E => F)(implicit E: Equal[C =>: F]): Boolean = {
      E.equal(dimap(dimap(gad)(fba)(fde))(fac)(fef), dimap(gad)(fba compose fac)(fef compose fde))
    }
  }

  def profunctorLaw: ProfunctorLaw = new ProfunctorLaw {}


}

object Profunctor {

  @inline def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Profunctor[G]): Profunctor[F] =
    new IsomorphismProfunctor[F, G] {
      override def G: Profunctor[G] = E

      override def iso: F <~~> G = D
    }
}


trait IsomorphismProfunctor[F[_, _], G[_, _]] extends Profunctor[F] {

  implicit def G: Profunctor[G]

  def iso: F <~~> G

  override def mapfst[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    iso.from(G.mapfst(iso.to(fab))(f))

  override def mapsnd[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    iso.from(G.mapsnd(iso.to(fab))(f))


}
