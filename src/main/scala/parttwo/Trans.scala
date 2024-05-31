package parttwo

import partone.ReaderFunctor

trait NaturalTrans[F[_], G[_]] {

  def apply[A](fa: F[A]): G[A]


}

trait RepresentableFunctor[F[_], X] {

  def tabulate[A](f: X => A): F[A]

  def index[A](fx: F[A]): X => A

}


case class Stream[X](h: () => X, t: () => Stream[X])


object Test extends App {


  implicit val intStreamRep: RepresentableFunctor[Stream, Int] = new RepresentableFunctor[Stream, Int] {

    override def tabulate[A](f: Int => A): Stream[A] =
      Stream(() => f(0), () => tabulate(((a: Int) => a + 1) andThen f))

    override def index[A](fx: Stream[A]): Int => A = (a: Int) =>
      fx match {
        case Stream(h, t) =>
          if (a == 0) h()
          else index(t())(a - 1)
      }
  }

  implicit def readerFunctorRep[E]: RepresentableFunctor[Function[E, *], E] = new RepresentableFunctor[E => *, E] {

    override def tabulate[A](f: E => A): E => A = f

    override def index[A](fx: E => A): E => A = fx
  }




}





