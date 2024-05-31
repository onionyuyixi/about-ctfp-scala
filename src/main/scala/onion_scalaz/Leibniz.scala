
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



}


object Leibniz {

  implicit def reflex[A]: Leibniz[A, A, A, A] = new Leibniz[A, A, A, A] {
    override def apply(a: A): A = a

    override def subst[F[_ >: A <: A]](p: F[A]): F[A] = p
  }

  def trans[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H](f: Leibniz[L, H, A, B], g: Leibniz[L, H, B, C]): Leibniz[L, H, A, C] = {

    g.subst[λ[`X >: L <: H` => Leibniz[L, H, A, X]]](f)
    val func: Leibniz[L, H, A, B] => Leibniz[L, H, A, C] = g.subst[λ[`X >: L <: H` => Leibniz[L, H, A, X]]]
    func(f)
  }

  def symm[L, H >: L, A >: L <: H, B >: L <: H](leibniz: Leibniz[L, H, A, B]): Leibniz[L, H, B, A] = {
    val function: Leibniz[L, H, A, A] => Leibniz[L, H, B, A] = leibniz.subst[λ[`X >: L <: H` => Leibniz[L, H, X, A]]]
    function(reflex)
  }




}