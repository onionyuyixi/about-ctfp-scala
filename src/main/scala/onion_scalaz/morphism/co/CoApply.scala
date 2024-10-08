package onion_scalaz.morphism.co

import onion_scalaz.<~>
import scalaz.Equal

trait CoApply[F[_]] extends Contravariant[F] {
  self =>

  def divide2[A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2)): F[Z]

  final def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] =
    divide2(fa, fb)(f)

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    contramap(a1)(f)

  def tuple2[A1, A2](a1: => F[A1], a2: => F[A2]): F[(A1, A2)] =
    divide2(a1, a2)(identity)

  def divide3[A1, A2, A3, Z](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: Z => (A1, A2, A3)): F[Z] =
    divide2(tuple2(a1, a2), a3) {
      z =>
        val tuple = f(z)
        ((tuple._1, tuple._2), tuple._3)
    }

  def divide4[A1, A2, A3, A4, Z](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] =
    divide2(tuple2(a1, a2), tuple2(a3, a4)) {
      z =>
        val t = f(z)
        ((t._1, t._2), (t._3, t._4))
    }

  final def dividing1[A1, Z](f: Z => A1)(implicit a1: F[A1]): F[Z] =
    divide1(a1)(f)

  final def dividing2[A1, A2, Z](f: Z => (A1, A2))(implicit a1: F[A1], a2: F[A2]): F[Z] =
    divide2(a1, a2)(f)


  final def dividing3[A1, A2, A3, Z](f: Z => (A1, A2, A3))(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    divide3(a1, a2, a3)(f)

  final def dividing4[A1, A2, A3, A4, Z](f: Z => (A1, A2, A3, A4))
                                        (implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    divide4(a1, a2, a3, a4)(f)

  trait DivideLaw extends ContravariantLaw {

    protected[this] def delta[A]: A => (A, A) = a => (a, a)

    def composition[A](a1: F[A], a2: F[A], a3: F[A])(implicit E: Equal[F[A]]): Boolean = {
      val x = divide(divide(a1, a2)(delta[A]), a3)(delta[A])
      val y = divide(a1, divide(a2, a3)(delta[A]))(delta[A])
      E.equal(x, y)
    }
  }

  def divideLaw: DivideLaw = new DivideLaw {}

}

object Divide {
  @inline def apply[F[_]](implicit F: CoApply[F]): CoApply[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: CoApply[G]): CoApply[F] =
    new IsomorphismCoApply[F, G] {
      override def G: CoApply[G] = E

      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCoApply[F[_], G[_]] extends CoApply[F] with IsomorphismContravariant[F, G] {
  implicit def G: CoApply[G]
  ////

  override def divide2[A, B, C](fa: => F[A], fb: => F[B])(f: C => (A, B)): F[C] =
    iso.from(G.divide(iso.to(fa), iso.to(fb))(f))
  ////
}

