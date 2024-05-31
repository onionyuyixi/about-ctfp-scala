package partone

trait Functor[F[_]] {


  //   lift function f
  def fmap0[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => fmap(f)(fa)

  def fmap[A, B](f: A => B)(fa: F[A]): F[B]

}

trait BiFunctor[F[_, _]] {

  def bifmap[A, B, C, D](f: A => C)(g: B => D): F[A, B] => F[C, D]

  def first[A, B, C](f: A => C): F[A, B] => F[C, B] = bifmap(f)(identity[B])

  def second[A, B, D](g: B => D): F[A, B] => F[A, D] = bifmap(identity[A])(g)


}

trait Reader[R] {

  def fmap0[A, B]: (A => B) => (R => A) => (R => B) = (f: A => B) => (g: R => A) => f compose g

}

trait ReaderFunctor[F[_]] {
  def fmap[A, B, R](f: A => B)(g: R => A)(fr: F[R]): F[B]
}


// BF 两个type params 一则以F[_] 一则以G[_]
case class BiComp[BF[_, _], F[_], G[_], A, B](v: BF[F[A], G[B]])

trait BiFunctorComp[BF[_, _], F[_], G[_]] {
  def map[A, B, C, D](f: F[A] => F[C])(g: G[B] => G[D]): BiComp[BF, F, G, A, B] => BiComp[BF, F, G, C, D]
}

case class Const[C, A](run: C => A) {self =>
  def fmap[B](f: A => B): Const[C, B] = Const((c: C) => f(self.run(c)))
}

trait Tree[A]

case class Leaf[A](a: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Functor {

  type Id[A] = A

  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    override def fmap[A, B](f: A => B)(fa: Id[A]): Id[B] = f(fa)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](f: A => B)(fa: Option[A]): Option[B] = fa.map(f)
  }

  implicit val optReaderFunctor: ReaderFunctor[Option] = new ReaderFunctor[Option] {

    override def fmap[A, B, R](f: A => B)(g: R => A)(fr: Option[R]): Option[B] =
      fr map (f compose g)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {

    override def fmap[A, B](f: A => B)(fa: Tree[A]): Tree[B] = fa match {
      case Leaf(a) => Leaf(f(a))
      case Node(left, right) => Node(fmap(f)(left), fmap(f)(right))
    }
  }

  implicit val tuple2BiFunction: BiFunctor[Tuple2] = new BiFunctor[Tuple2] {

    override def bifmap[A, B, C, D](f: A => C)(g: B => D): ((A, B)) => (C, D) = {
      case (a, b) => (f(a), g(b))
    }
  }


  implicit val eitherBiFunction: BiFunctor[Either] = new BiFunctor[Either] {

    override def bifmap[A, B, C, D](f: A => C)(g: B => D): Either[A, B] => Either[C, D] = {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }


  implicit def biCompFunctor[BF[_, _], F[_], G[_]](implicit bf: BiFunctor[BF],
                                                   fFunctor: Functor[F],
                                                   gFunctor: Functor[G],
                                                  ) = {

    type BiCompAB[A, B] = BiComp[BF, F, G, A, B]
    new BiFunctor[BiCompAB] {
      override def bifmap[A, B, C, D](f: A => C)(g: B => D): BiCompAB[A, B] => BiCompAB[C, D] = {
        case BiComp(v) =>
          val ff: F[A] => F[C] = fFunctor.fmap(f)
          val gg: G[B] => G[D] = gFunctor.fmap(g)
          val vv: BF[F[A], G[B]] = v
          BiComp(bf.bifmap(ff)(gg)(vv))
      }
    }

  }


  implicit val listOptBiFunctorComp: BiFunctorComp[Either, List, Option] = new BiFunctorComp[Either, List, Option] {
    override def map[A, B, C, D](f: List[A] => List[C])(g: Option[B] => Option[D]):
    BiComp[Either, List, Option, A, B] => BiComp[Either, List, Option, C, D] = {
      case BiComp(v) =>
        v match {
          case Left(a) => BiComp(Left(f(a)))
          case Right(b) => BiComp(Right(g(b)))
        }
    }
  }


}


object TestFunctor extends App {

  def square: Int => Int = (a: Int) => a * a

  val list = List(1, 2, 3)
  val result = Functor.optionFunctor.fmap(Functor.listFunctor.fmap(square))(Option(list))
  println(result)

  private val maybeBoolean = Functor
    .optReaderFunctor
    .fmap((a: Int) => a == 10)((a: String) => Integer.parseInt(a))(Some("10"))

  println(maybeBoolean.get)

  println(Functor.eitherBiFunction.bifmap((a: Int) => a + 1)((b: Int) => b - 1)(Left(1)))

  println(Functor.eitherBiFunction.bifmap((a: Int) => a + 1)((b: Int) => b - 1)(Right(1)))

  println(Functor.idFunctor.fmap((a: Int) => String.valueOf(a) + " test idFunctor")(1))


}
