package onion_scalaz



sealed abstract class Liskov[-A, +B] {

  // 最基本的函数关系 A=>B
  def apply(a: A): B = Liskov.witness(this)(a)

  // covariant或者contravariant 替换是在Functor 容器下进行的
  // 函数形式是F[A]=>F[B] functor的lift作用
  def covariant[F[+_]](p: F[A]): F[B]

  // 函数形式是F[A]=>F[B] cofunctor的lift作用
  def contravariant[F[-_]](p: F[B]): F[A]

  def andThen[C](g: Liskov[B, C]): A <~< C = {
    val thisInstance: Liskov[A, B] = this
    val func: Liskov[B, C] => Liskov[A, C] = thisInstance.contravariant[Liskov[-*, C]]
    func(g)
  }

  def compose[C](g: Liskov[C, A]): Liskov[C, B] = {
    val func: Liskov[C, A] => Liskov[C, B] = this.covariant[<~<[C, +*]]
    func(g)
  }


}

object Liskov {


  implicit def witness[A, B](lt: A <~< B): A => B = {
    val func: (B => B) => A => B = lt.contravariant[-* => B]
    func(identity)
  }

  implicit def refl[A]: (A <~< A) = new(A <~< A) {

    override def covariant[F[+_]](p: F[A]): F[A] = p

    override def contravariant[F[-_]](p: F[A]): F[A] = p
  }

  implicit val liskov: Category[<~<] = new Category[<~<] {
    override def id[A]: A <~< A = refl

    override def compose[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
      f.compose(g)
  }

  def trans[A, B, C](f: Liskov[A, B], g: Liskov[B, C]): Liskov[A, C] = {

    val func: B <~< C => A <~< C = f.contravariant[<~<[-*, C]]
    func(g)

    val fun1: A <~< B => A <~< C = g.covariant[<~<[A, +*]]
    fun1.apply(f)

  }


  def fromLeibniz[A, B](ev: A === B): Liskov[A, B] = new(A <~< B) {
    override def covariant[F[+_]](p: F[A]): F[B] = ev.subst(p)

    override def contravariant[F[-_]](p: F[B]): F[A] = ev.flip.subst(p)
  }


}



