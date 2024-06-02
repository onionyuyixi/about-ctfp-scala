package onion_scalaz.morphism

import scalaz.{Equal}


trait Apply[F[_]] extends Functor[F] {
  self =>

  // 按 这里的f 表达是 在F 中的函数A=>B 这个时候在F中的元素以函数为节点
  // 而F[A]=>F[B] 表达的是 A=>B 在被lift 到F 中 未改变A=>B的原有映射
  // ap方法的两个参数否在F中 是一种未切换容器的计算
  // 与Functor[F]中的map不同 map(fa:F[A])(f:A=>B):F[B] 这是lift effect 存在容器的切换
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]


  def traverse1[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse1[G]): F[G[B]] =
    G.traverse1(value)(f)(this)

  def sequence1[A, G[_]: Traverse1](as: G[F[A]]): F[G[A]] =



  trait ApplyLaw extends FunctorLaw {
    /** Lifted functions can be fused. */
    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(ap(ap(fa)(fab))(fbc),
        ap(fa)(ap(fab)(map(fbc)((bc: B => C) => (ab: A => B) => bc compose ab))))
  }

}


object Apply {

  @inline def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

}
