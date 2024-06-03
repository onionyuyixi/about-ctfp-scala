package onion_scalaz.monoid

import onion_scalaz.<=>
import onion_scalaz.category_base.Category
import onion_scalaz.morphism.InvariantFunctor
import scalaz.{Equal, Maybe}

// 类似Category[=>:[_,_]]
trait Monoid[F] extends Semigroup[F] {
  self =>

  // 类似于Category 中的 id方法
  def zero: F

  def isMZero(a: F)(implicit eq: Equal[F]): Boolean =
    eq.equal(a, zero)

  final def ifEmpty[B](a: F)(t: => B)(f: => B)(implicit eq: Equal[F]): B =
    if (isMZero(a)) {
      t
    } else f


  final def onNotEmpty[B](a: F)(v: => B)(implicit eq: Equal[F], mb: Monoid[B]): B =
    if (isMZero(a)) {
      mb.zero
    } else v


  final def onEmpty[A, B](a: F)(v: => B)(implicit eq: Equal[F], mb: Monoid[B]): B =
    ifEmpty(a)(mb.zero)(v)


  def unfoldlSum[S](seed: S)(f: S => Maybe[(S, F)]): F =
    unfoldlSumOpt(seed)(f) getOrElse zero

  def unfoldrSum[S](seed: S)(f: S => Maybe[(F, S)]): F =
    unfoldrSumOpt(seed)(f) getOrElse zero

  final def category: Category[λ[(a, b) => F]] = new Category[λ[(a, b) => F]] with SemigroupCompose {
    override def id[A]: F = zero
  }

  trait MonoidLaw extends SemigroupLaw {
    def leftIdentity(a: F)(implicit F: Equal[F]): Boolean = F.equal(a, append(zero, a))

    def rightIdentity(a: F)(implicit F: Equal[F]): Boolean = F.equal(a, append(a, zero))
  }


}


object Monoid {

  @inline def apply[F](implicit mf: Monoid[F]): Monoid[F] = mf


  def fromIso[F, G](D: F <=> G)(implicit M: Monoid[G]): Monoid[F] = new Monoid[F] {
    override def zero: F = D.from(M.zero)

    override def append(f1: F, f2: => F): F = D.from(M.append(D.to(f1), D.to(f2)))
  }

  def instance[A](f: (A, => A) => A, z: A): Monoid[A] = new Monoid[A] {
    override def zero: A = z

    override def append(f1: A, f2: => A): A = f.curried(f1)(f2)
  }


  implicit val monoidInvariantFunctor: InvariantFunctor[Monoid] =
    new InvariantFunctor[Monoid] {
      def xmap[A, B](ma: Monoid[A], f: A => B, g: B => A): Monoid[B] = new Monoid[B] {
        def zero: B = f(ma.zero)

        def append(x: B, y: => B): B = f(ma.append(g(x), g(y)))
      }
    }

}


trait IsomorphismMonoid[F, G] extends Monoid[F] with IsomorphismSemigroup[F, G] {
  implicit def G: Monoid[G]
  ////

  def zero: F = iso.from(G.zero)
  ////
}