
package onion_scalaz


trait Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H] {


  // apply 表示Leibniz A B 之间（没有Functor）之间的最基础的函数关系 A=>B
  def apply(a: A): B = {
    val func: Id[A] => Id[B] = subst[Id]
    func.apply(a)
  }


  def substId(p: Id[A]): Id[B] = subst(p)

  // substitution的函数类型 F[A]=>F[B] 这就是Functor life function的作用
  def subst[F[_ >: L <: H]](p: F[A]): F[B]


  def compose[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, C, A]): Leibniz[L2, H2, C, B]
  = Leibniz.trans(that, this)

  def andThen[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, B, C]): Leibniz[L2, H2, A, C]
  = Leibniz.trans(this, that)

  def flip: Leibniz[L, H, B, A] = Leibniz.symm(this)

  // X=>A 相当于 Funtion1[X,A]
  def onF[X](fa: X => A): X => B = {
    val func: (X => A) => X => B = subst[X => *]
    func(fa)
  }


  def liskov: Liskov[A, B] = Liskov.fromLeibniz(this)


}


object Leibniz {

  implicit def reflex[A]: Leibniz[A, A, A, A] = new Leibniz[A, A, A, A] {
    override def apply(a: A): A = a

    override def subst[F[_ >: A <: A]](p: F[A]): F[A] = p
  }

  implicit def subst[A, B](a: A)(implicit f: A === B): B = f.subst[Id](a)


  def trans[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H](f: Leibniz[L, H, A, B], g: Leibniz[L, H, B, C]): Leibniz[L, H, A, C] = {

    g.subst[λ[`X >: L <: H` => Leibniz[L, H, A, X]]](f)
    val func: Leibniz[L, H, A, B] => Leibniz[L, H, A, C] = g.subst[λ[`X >: L <: H` => Leibniz[L, H, A, X]]]
    func(f)
  }

  def symm[L, H >: L, A >: L <: H, B >: L <: H](leibniz: Leibniz[L, H, A, B]): Leibniz[L, H, B, A] = {
    val function: Leibniz[L, H, A, A] => Leibniz[L, H, B, A] = leibniz.subst[λ[`X >: L <: H` => Leibniz[L, H, X, A]]]
    function(reflex)
  }


  def lift[LA, LT,
    HA >: LA, HT >: LT,
    T[_ >: LA <: HA] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA
  ](a: Leibniz[LA, HA, A, A2]): Leibniz[LT, HT, T[A], T[A2]] = {

    val func: Leibniz[LT, HT, T[A], T[A]] => Leibniz[LT, HT, T[A], T[A2]] =
      a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A], T[X]]]]

    func(reflex)
  }


  def lift2[
    LA, LB, LT,
    HA >: LA, HB >: LB, HT >: LT,
    T[_ >: LA <: HA, _ >: LB <: HB] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA,
    B >: LB <: HB, B2 >: LB <: HB
  ](
     a: Leibniz[LA, HA, A, A2],
     b: Leibniz[LB, HB, B, B2]
   ): Leibniz[LT, HT, T[A, B], T[A2, B2]] = {

    val func1: Leibniz[LT, HT, T[A, B], T[A, B]] => Leibniz[LT, HT, T[A, B], T[A2, B]] =
      a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A, B], T[X, B]]]]

    val v1: Leibniz[LT, HT, T[A, B], T[A2, B]] = func1(reflex)

    val func2: Leibniz[LT, HT, T[A, B], T[A2, B]] => Leibniz[LT, HT, T[A, B], T[A2, B2]] =
      b.subst[λ[`X >: LB <: HB` => Leibniz[LT, HT, T[A, B], T[A2, X]]]]
    func2(v1)

  }


  def lift3[
    LA, LB, LC, LT,
    HA >: LA, HB >: LB, HC >: LC, HT >: LT,
    T[_ >: LA <: HA, _ >: LB <: HB, _ >: LC <: HC] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA,
    B >: LB <: HB, B2 >: LB <: HB,
    C >: LC <: HC, C2 >: LC <: HC
  ](
     a: Leibniz[LA, HA, A, A2],
     b: Leibniz[LB, HB, B, B2],
     c: Leibniz[LC, HC, C, C2]
   ): Leibniz[LT, HT, T[A, B, C], T[A2, B2, C2]] = {

    val func1: Leibniz[LT, HT, T[A, B, C], T[A, B, C]] => Leibniz[LT, HT, T[A, B, C], T[A2, B, C]] =
      a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A, B, C], T[X, B, C]]]]
    val v1: Leibniz[LT, HT, T[A, B, C], T[A2, B, C]] = func1(reflex)

    val func2: Leibniz[LT, HT, T[A, B, C], T[A2, B, C]] => Leibniz[LT, HT, T[A, B, C], T[A2, B2, C]] =
      b.subst[λ[`X >:LB <: HB` => Leibniz[LT, HT, T[A, B, C], T[A2, X, C]]]]
    val v2: Leibniz[LT, HT, T[A, B, C], T[A2, B2, C]] = func2(v1)

    val func3: Leibniz[LT, HT, T[A, B, C], T[A2, B2, C]] => Leibniz[LT, HT, T[A, B, C], T[A2, B2, C2]] =
      c.subst[λ[`X >: LC <: HC` => Leibniz[LT, HT, T[A, B, C], T[A2, B2, X]]]]
    func3(v2)

  }

  def force[L, H >: L, A >: L <: H, B >: L <: H]: Leibniz[L, H, A, B] = new Leibniz[L, H, A, B] {
    def subst[F[_ >: L <: H]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
  }


  def lower[
    LA, HA >: LA,
    T[_ >: LA <: HA] /*: Injective*/ ,
    A >: LA <: HA, A2 >: LA <: HA
  ](t: ===[T[A], T[A2]]): Leibniz[LA, HA, A, A2] = force[LA, HA, A, A2]


}











