package onion_scalaz.morphism.functionusage

import onion_scalaz.morphism.Functor
import scalaz.Equal


//Representable functors, that is to say, those with isomorphisms to and from [a](X => a).
// As such, all typeclasses and operations on [a](X => a),
// that is, fixed in X, can be trivially derived for F.
abstract class Representable[F[_], X](implicit val F: Functor[F]) {

  // 考虑到F[X]  rep方法实际表达的意思是 F[X]=>(X=>A)=>F[A]
  // 这跟Functor的map 寓意相同
  def rep[A](f: X => A): F[A]

  // unrep的寓意 F[X]=>F[A]=>(X=>A)
  def unrep[A](fa: F[A]): X => A


  trait RepresentableLaw {
    /** `rep compose unrep` is vacuous. */
    def repUnrep[A](f: F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(rep(unrep(f)), f)

    /** `unrep compose rep` is vacuous. */
    def unrepRep[A](f: X => A, x: X)(implicit E: Equal[A]): Boolean =
      E.equal(unrep(rep(f))(x), f(x))
  }

  def representableLaw: RepresentableLaw = new RepresentableLaw {}

}


sealed abstract class RepresentableInstances {


  implicit def readerRepresentable[E]: Representable[E => *, E] =
    new Representable[E => *, E] {
      def rep[A](f: E => A): E => A = f

      def unrep[A](f: E => A): E => A = f
    }

  implicit val f0Representable: Representable[Function0, Unit] =
    new Representable[Function0, Unit] {
      def rep[A](f: Unit => A): () => A = () => f(())

      def unrep[A](f: () => A): Unit => A = u => f()
    }

  implicit def curryRepresentable[E]: Representable[E => *, (E, Unit)] =
    new Representable[E => *, (E, Unit)] {
      def rep[A](f: ((E, Unit)) => A): E => A = e => f(e, ())

      def unrep[A](f: E => A): ((E, Unit)) => A = e => f(e._1)
    }

}