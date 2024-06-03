package onion_scalaz.morphism

import onion_scalaz.category_base.{Category, Compose}
import onion_scalaz.morphism.Kleisli.kleisli
import onion_scalaz.~>

// type的含义 A=>M[B]
final case class Kleisli[M[_], A, B](run: A => M[B]) {
  self =>

  def apply(a: A): M[B] = run(a)

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
    Kleisli((a: A) => f(run(a)))


  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] =
    kleisli((a: A) => b.bind(this (a))(k.run))


}


object Kleisli {

  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] =
    Kleisli(f)

  implicit def kleisliCompose[F[_]](implicit F0: Bind[F]): Compose[Kleisli[F, *, *]] =
    new KleisliCompose[F] {
      override def F: Bind[F] = F0
    }

  //todo to imply
  implicit def kleisliCategory[F[_]](implicit F0: Monad[F]): Category[Kleisli[F, *, *]] =
    new Category[Kleisli[F, *, *]] {

      override def id[A]: Kleisli[F, A, A] = ???

      override def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] = ???
    }
}


