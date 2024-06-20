package onion_scalaz.morphism

import onion_scalaz.<~>
import onion_scalaz.morphism.functors.{Functor, IsomorphismFunctor, ProductApply}
import scalaz.{Equal, Id}


trait Apply[F[_]] extends Functor[F] {
  self =>

  // 按 这里的f 表达是 在F 中的函数A=>B 这个时候在F中的元素以函数为节点
  // 而F[A]=>F[B] 表达的是 A=>B 在被lift 到F 中 未改变A=>B的原有映射
  // ap方法的两个参数否在F中 是一种未切换容器的计算
  // 与Functor[F]中的map不同 map(fa:F[A])(f:A=>B):F[B] 这是lift effect 存在容器的切换
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  // apF实际上是ap方法一种抽象
  // 容器总morphism A=>B 变成了容器中数据之间的morphism
  def apF[A, B](f: => F[A => B]): F[A] => F[B] = ap(_)(f)


  def ap2[A, B, C](fa: => F[A], fb: => F[B])(f: F[(A, B) => C]): F[C] = {
    ap(fb)(ap(fa)(map(f)(_.curried)))

    // 具体步骤   首先curry化 得到一个 以morphism 为元素的F[_]
    // 其次按照 curry化后的函数 再逐次求解
    val func1: F[A => B => C] = map(f)(_.curried)
    val fb2c: F[B => C] = ap(fa)(func1)
    ap(fb)(fb2c)

  }


  def ap3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A, B, C) => D]): F[D] = {
    val func1: F[A => B => C => D] = map(f)(_.curried)
    val func2: F[B => C => D] = ap(fa)(func1)
    ap(fc)(ap(fb)(func2))
  }


  def ap4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A, B, C, D) => E]): F[E] =
    ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried)))))

  // ap方法有很多衍生方法apN[_,_,...,_] 跟ap2一样  只是嵌套的更多了些
  // 同样apply方法有很多衍生方法applyN[_,_,...,_] 跟apply2一样  只是嵌套的更多了些

  // lift2方法做出了正确理解示范  将函数 (A,B)=>C lift 成 (F[A],F[B])=>F[C]
  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    apply2(_, _)(f)

  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = {
    // 使用了Functor的map方法 就是原函数被lift到新范畴F中
    val func1: F[B => C] = map(fa)(f.curried)
    ap(fb)(func1)
  }

  def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    ap(fc)(ap(fb)(map(fa)(f.curried)))

  def apply4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))


  // tuple方法是apply的特殊形式
  def tuple2[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] =
    apply2(fa, fb)((_,_))

  def tuple3[A, B, C](fa: => F[A], fb: => F[B], fc: => F[C]): F[(A, B, C)] =
    apply3(fa, fb, fc)((_, _, _))

  def tuple4[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D]): F[(A, B, C, D)] =
    apply4(fa, fb, fc, fd)((_, _, _, _))

  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    apply3(_, _, _)(f)

  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    apply4(_, _, _, _)(f)

  // 抛弃掉左边
  def discardLeft[A, B](fa: => F[A], fb: => F[B]): F[B] = apply2(fa, fb)((_, b) => b)

  def discardRight[A, B](fa: => F[A], fb: => F[B]): F[A] = apply2(fa, fb)((a, _) => a)


  def compose[G[_]](implicit G0: Apply[G]): Apply[λ[a => F[G[a]]]] = new ApplyComposition[F, G] {
    override implicit def F: Apply[F] = self

    override implicit def G: Apply[G] = G0

  }


  def product[G[_]](implicit G0: Apply[G]): Apply[λ[α => (F[α], G[α])]] =
    new ProductApply[F, G] {
      override def F: Apply[F] = self

      override def G: Apply[G] = G0
    }

  def flip: Apply[F] = new FlippedApply {}


  protected[this] trait FlippedApply extends Apply[F] {

    override def map[A, B](fa: F[A])(f: A => B): F[B] = self.map(fa)(f)

    override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = {
      val func: F[(A => B) => B] = self.map(fa)(a => (a2b: A => B) => a2b(a))
      self.ap(f)(func)
    }

    override def flip: self.type = self

  }








  //  todo
  //  def traverse1[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse1[G]): F[G[B]] =
  //    G.traverse1(value)(f)(this)
  //
  //  def sequence1[A, G[_]: Traverse1](as: G[F[A]]): F[G[A]] =


  trait ApplyLaw extends FunctorLaw {
    /** Lifted functions can be fused. */
    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A])(implicit FC: Equal[F[C]])
    : Boolean = FC.equal(ap(ap(fa)(fab))(fbc),
      ap(fa)(ap(fab)(map(fbc)((bc: B => C) => (ab: A => B) => bc compose ab))))
  }

}


object Apply {

  @inline def apply[F[_]](implicit F: Apply[F]): Apply[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Apply[G]): Apply[F] = new IsomorphismApply[F, G] {
    override implicit def G: Apply[G] = E

    override def iso: F <~> G = D
  }

}


trait IsomorphismApply[F[_], G[_]] extends IsomorphismFunctor[F, G] with Apply[F] {

  implicit def G: Apply[G]

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
    iso.from(G.ap(iso.to(fa))(iso.to(f)))

  override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = {
    val func: ((A, B) => C) => G[C] = G.apply2(iso.to(fa), iso.to(fb))
    iso.from(func(f))
  }
}
