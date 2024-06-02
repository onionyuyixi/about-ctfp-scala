package onion_scalaz.morphism

import onion_scalaz.~>

// type的含义 A=>M[B]
final case class Kleisli[M[_], A, B](run: A => M[B]) {
  self =>

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] =
    Kleisli(a => M.map(run(a))(f))

  // dimap 可以通过map 来进行转换实现
  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[M]): Kleisli[M, C, D] = {
    Kleisli((c: C) => b.map(run(f(c)))(g))
    // 用已经有的map进行实现
    val kleisli_ : Kleisli[M, A, D] = map(g)(b)
    Kleisli((c: C) => kleisli_.run(f(c)))
    // map的简化版
    Kleisli(c => map(g)(b).run(f(c)))
  }


  // f 函数 转换了Functor 同时保持了B=>C 的映射
  def mapT[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] = {
    Kleisli((a: A) => f(run(a)))
    //简化版
    Kleisli(run andThen f)
  }

  def transform[N[_]](f: M ~> N): Kleisli[N, A, B] =
    Kleisli((a:A)=>f(run(a)))




}

object Kleisli {


}
