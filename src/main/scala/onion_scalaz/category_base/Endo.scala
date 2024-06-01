package onion_scalaz


// 类似于Id[X]
final case class Endo[A](run: A => A) {


  def apply(a: A): A = run(a)

  def compose(other: Endo[A]): Endo[A] = Endo.endo(run compose other.run)

  def andThen(other: Endo[A]): Endo[A] = Endo.endo(run andThen other.run)


}

sealed abstract class EndoInstances {

  implicit def monoidEndo[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {

    override def zero: Endo[A] = Endo.idEndo[A]

    override def append(f1: Endo[A], f2: => Endo[A]): Endo[A] = f1 compose f2
  }

  def unzip[A, B](a: Endo[(A, B)]): (Endo[A], Endo[B]) = {
    val aToa: A => A = (x: A) => a((x, null.asInstanceOf[B]))._1
    val bTob: B => B = (x: B) => a((null.asInstanceOf[A], x))._2
    (Endo(aToa), Endo(bTob))
  }

}

final case class EndoByName[A](run: (=> A) => A) {

  def apply(a: => A): A = run(a)

  def compose(other: => EndoByName[A]): EndoByName[A] = EndoByName(x => run(other.run(x)))

  def andThen(other: EndoByName[A]): EndoByName[A] = other compose this

}


object Endo extends EndoInstances {

  final def endo[A](f: A => A): Endo[A] = Endo(f)

  final def idEndo[A]: Endo[A] = endo[A](a => a)

  def IsoEndo[A]: Endo[A] <=> (A => A) = new IsoSet[Endo[A], A => A] {
    override def to: Endo[A] => A => A = (x: Endo[A]) => x.run

    override def from: (A => A) => Endo[A] = (y: A => A) => endo(y)
  }

  private val IsoFunctorEndo: IsoFunctorTemplate[Endo, λ[x => x => x]] = new IsoFunctorTemplate[Endo, λ[x => x => x]] {

    override def to_[A](fa: Endo[A]): A => A = fa.run

    override def from_[A](ga: A => A): Endo[A] = Endo(ga)
  }

}

