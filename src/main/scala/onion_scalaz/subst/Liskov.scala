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
    val func: Liskov[B, C] => Liskov[A, C] = thisInstance.contravariant[* <~< C]
    func(g)
  }

  def compose[C](g: Liskov[C, A]): Liskov[C, B] = {
    val func: Liskov[C, A] => Liskov[C, B] = this.covariant[Liskov[C, *]]
    func(g)
  }


}

object Liskov {


  implicit def witness[A, B](lt: A <~< B): A => B = {
    val func: (B => B) => A => B = lt.contravariant[* => B]
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

    val func: B <~< C => A <~< C = f.contravariant[Liskov[*, C]]
    func(g)

    val fun1: A <~< B => A <~< C = g.covariant[Liskov[A, +*]]
    fun1.apply(f)

  }


  def co[T[+_], A, A2](a: A <~< A2): T[A] <~< T[A2] = {
    val func1: T[A2] <~< T[A2] => T[A] <~< T[A2] = a.contravariant[λ[`+x` => T[x] <~< T[A2]]]
    func1(refl)
  }

  def co2[T[+_, _], Z, A, B](a: A <~< Z): T[A, B] <~< T[Z, B] = {
    val func: T[Z, B] <~< T[Z, B] => T[A, B] <~< T[Z, B] = a.contravariant[λ[`+x` => T[x, B] <~< T[Z, B]]]
    func(refl)
  }

  def co2_2[T[_, +_], Z, A, B](a: B <~< Z): T[A, B] <~< T[A, Z] = {

    val func: T[A, Z] <~< T[A, Z] => T[A, B] <~< T[A, Z] = a.contravariant[λ[`+x` => T[A, x] <~< T[A, Z]]]

    func(refl)
  }

  def co3[T[+_, _, _], Z, A, B, C](a: A <~< Z): T[A, B, C] <~< T[Z, B, C] = {
    a.contravariant[λ[`+x` => T[x, B, C] <~< T[Z, B, C]]](refl)
  }

  def co4[T[+_, _, _, _], Z, A, B, C, D](a: A <~< Z): T[A, B, C, D] <~< T[Z, B, C, D] =
    a.contravariant[λ[`+x` => T[x, B, C, D] <~< T[Z, B, C, D]]](refl)

  /** lift2(a,b) = co1_2(a) compose co2_2(b) */
  def lift2[T[+_, +_], A, A2, B, B2](a: A <~< A2, b: B <~< B2): T[A, B] <~< T[A2, B2] = {
    type a[-X] = T[X, B2] <~< T[A2, B2]
    type b[-X] = T[A, X] <~< T[A2, B2]
    b.contravariant[b](a.contravariant[a](refl))

  }

  /** lift3(a,b,c) = co1_3(a) compose co2_3(b) compose co3_3(c) */
  def lift3[T[+_, +_, +_], A, A2, B, B2, C, C2](
                                                 a: A <~< A2,
                                                 b: B <~< B2,
                                                 c: C <~< C2
                                               ): T[A, B, C] <~< T[A2, B2, C2] = {
    type a[-X] = T[X, B2, C2] <~< T[A2, B2, C2]
    type b[-X] = T[A, X, C2] <~< T[A2, B2, C2]
    type c[-X] = T[A, B, X] <~< T[A2, B2, C2]
    c.contravariant[c](b.contravariant[b](a.contravariant[a](refl)))
  }

  /** lift4(a,b,c,d) = co1_3(a) compose co2_3(b) compose co3_3(c) compose co4_4(d) */
  def lift4[T[+_, +_, +_, +_], A, A2, B, B2, C, C2, D, D2](
                                                            a: A <~< A2,
                                                            b: B <~< B2,
                                                            c: C <~< C2,
                                                            d: D <~< D2
                                                          ): T[A, B, C, D] <~< T[A2, B2, C2, D2] = {
    type a[-X] = T[X, B2, C2, D2] <~< T[A2, B2, C2, D2]
    type b[-X] = T[A, X, C2, D2] <~< T[A2, B2, C2, D2]
    type c[-X] = T[A, B, X, D2] <~< T[A2, B2, C2, D2]
    type d[-X] = T[A, B, C, X] <~< T[A2, B2, C2, D2]
    d.contravariant[d](c.contravariant[c](b.contravariant[b](a.contravariant[a](refl))))
  }


  def contra[T[-_], A, A2](a: A <~< A2): T[A2] <~< T[A] = {
    val function: T[A2] <~< T[A2] => T[A2] <~< T[A] = a.contravariant[λ[`-x` => T[A2] <~< T[x]]]
    function(refl)
  }

  // binary
  def contra1_2[T[-_, _], Z, A, B](a: A <~< Z): (T[Z, B] <~< T[A, B]) = {
    val func: T[Z, B] <~< T[Z, B] => T[Z, B] <~< T[A, B] =
      a.contravariant[λ[`+x` => T[Z, B] <~< T[x, B]]]
    func(refl)
  }

  def contra2_2[T[_, -_], Z, A, B](a: B <~< Z): (T[A, Z] <~< T[A, B]) = {
    val func: T[A, Z] <~< T[A, Z] => T[A, Z] <~< T[A, B] = a.contravariant[λ[`+x` => T[A, Z] <~< T[A, x]]]
    func(refl)
  }


  def contra1_3[T[-_, _, _], Z, A, B, C](a: A <~< Z): (T[Z, B, C] <~< T[A, B, C]) =
    a.contravariant[λ[`-x` => T[Z, B, C] <~< T[x, B, C]]](refl)

  def contra2_3[T[_, -_, _], Z, A, B, C](a: B <~< Z): (T[A, Z, C] <~< T[A, B, C]) =
    a.contravariant[λ[`-x` => T[A, Z, C] <~< T[A, x, C]]](refl)

  def contra3_3[T[_, _, -_], Z, A, B, C](a: C <~< Z): (T[A, B, Z] <~< T[A, B, C]) =
    a.contravariant[λ[`-x` => T[A, B, Z] <~< T[A, B, x]]](refl)

  def contra1_4[T[-_, _, _, _], Z, A, B, C, D](a: A <~< Z): (T[Z, B, C, D] <~< T[A, B, C, D]) =
    a.contravariant[λ[`-x` => T[Z, B, C, D] <~< T[x, B, C, D]]](refl)

  def contra2_4[T[_, -_, _, _], Z, A, B, C, D](a: B <~< Z): (T[A, Z, C, D] <~< T[A, B, C, D]) =
    a.contravariant[({type l[-x] = T[A, Z, C, D] <~< T[A, x, C, D]})#l](refl)

  def contra3_4[T[_, _, -_, _], Z, A, B, C, D](a: C <~< Z): (T[A, B, Z, D] <~< T[A, B, C, D]) =
    a.contravariant[({type l[-x] = T[A, B, Z, D] <~< T[A, B, x, D]})#l](refl)

  def contra4_4[T[_, _, _, -_], Z, A, B, C, D](a: D <~< Z): (T[A, B, C, Z] <~< T[A, B, C, D]) =
    a.contravariant[({type l[-x] = T[A, B, C, Z] <~< T[A, B, C, x]})#l](refl)

  def fromLeibniz[A, B](ev: A === B): Liskov[A, B] = new(A <~< B) {
    override def covariant[F[+_]](p: F[A]): F[B] = ev.subst(p)

    override def contravariant[F[-_]](p: F[B]): F[A] = ev.flip.subst(p)
  }


}



