package partone

trait Contravariant[F[_]] {

  def contramap[A, B](f: B => A)(fa: F[A]): F[B]

}

trait ProFunctor[F[_, _]] {

  def dimap[A, B, C, D](f: C => A)(g: B => D): F[A, B] => F[C, D]

  def lmap[A, B, C](f: C => A): F[A, B] => F[C, B]
  = dimap(f)(identity[B])

  def rmap[A, B, C](g: B => C): F[A, B] => F[A, C]
  = dimap(identity[A])(g)

}


object ProFunctor {


  implicit val function1ProFunctor: ProFunctor[Function] = new ProFunctor[Function1] {

    override def dimap[A, B, C, D](f: C => A)(g: B => D): (A => B) => C => D =
      fab => f.andThen(fab.andThen(g))
  }


}