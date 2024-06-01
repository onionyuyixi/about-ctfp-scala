
package onion_scalaz


trait Iso[Arr[_, _], A, B] {

  self =>

  def to: Arr[A, B]

  def from: Arr[B, A]

  def flip: Iso[Arr, B, A] = new Iso[Arr, B, A] {

    override def to: Arr[B, A] = self.from

    override def from: Arr[A, B] = self.to

    override def flip: Iso[Arr, A, B] = self
  }

}


trait FunctorIso[Arr[_[_], _[_]], F[_], G[_]] {

  self =>

  def to: Arr[F, G]

  def from: Arr[G, F]

}

trait BifunctorIso[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {

  self =>

  def to: Arr[F, G]

  def from: Arr[G, F]

}

trait IsoFunctorTemplate[F[_], G[_]] extends IsoFunctor[F, G] {

  override final val to: NaturalTrans[F, G] = new(F ~> G) {
    override def apply[A](fa: F[A]): G[A] = to_(fa)
  }

  override final val from: NaturalTrans[G, F] = new(F <~ G) {

    override def apply[A](ga: G[A]): F[A] = from_(ga)
  }

  def to_[A](fa: F[A]): G[A]

  def from_[A](ga: G[A]): F[A]


}

object Isos {

  implicit def functionIso[A, B](f: A => B, g: B => A): Iso[Function, A, B] = new Iso[Function1, A, B] {

    override def to: A => B = f

    override def from: B => A = g
  }

  implicit def functorIso[F[_], G[_]](fab: F[_] => G[_], fba: G[_] => F[_]): FunctorIso[NaturalTrans, F, G] =
    new FunctorIso[NaturalTrans, F, G] {

      override def to: NaturalTrans[F, G] = new NaturalTrans[F, G] {
        override def apply[A](fa: F[A]): G[A] = fab.apply(fa).asInstanceOf[G[A]]
      }

      override def from: NaturalTrans[G, F] = new NaturalTrans[G, F] {
        override def apply[A](ga: G[A]): F[A] = fba.apply(ga).asInstanceOf[F[A]]
      }

    }

  implicit def bifunctorIso[F[_, _], G[_, _]](fab: F[_, _] => G[_, _], fba: G[_, _] => F[_, _]): BifunctorIso[BiNaturalTrans, F, G] =
    new BifunctorIso[BiNaturalTrans, F, G] {


      override def to: BiNaturalTrans[F, G] = new BiNaturalTrans[F, G] {
        override def apply[A, B](fa: F[A, B]): G[A, B] = fab.apply(fa).asInstanceOf[G[A, B]]
      }

      override def from: BiNaturalTrans[G, F] = new BiNaturalTrans[G, F] {
        override def apply[A, B](ga: G[A, B]): F[A, B] = fba.apply(ga).asInstanceOf[F[A, B]]
      }
    }


  // commutative 表示可以flip
  def commutative[A, B](i: Iso[Function, A, B]): Iso[Function, B, A] = i.flip

  // reflexive 表示自己的映射
  def reflexive[A]: Iso[Function, A, A] = new Iso[Function, A, A] {
    override def to: Function[A, A] = a => a

    override def from: Function[A, A] = a => a
  }


}

