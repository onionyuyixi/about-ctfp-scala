package onion_scalaz.category_base

import onion_scalaz.monoid.{Monoid, Semigroup}
import onion_scalaz.morphism.co.{Cobind, Cokleisli, Comonad}
import onion_scalaz.morphism.{Bind, Kleisli, Monad}


// =>:[_, _]  表示Function Type
// A则是入参type Input Argue
// 其结果则是会产生一个新的type  [=>:,A]  相当于  [=>:,A] = (=>:(A))
// 同时结合Compose[=>:[_,_]] 比较 可以知道Compose[=>:[_,_]] 的意思是 FunctionType
final case class Endomorphic[=>:[_, _], A](run: =>:[A, A]) {


  def compose(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    Endomorphic(F.compose(run, that.run))

  def andThen(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    that.compose(this)

}

sealed abstract class EndomorphicInstances extends EndomorphicInstances0 {
  implicit def kleisliEndoInstance[F[_] : Monad, A]: Monoid[Endomorphic[Kleisli[F, *, *], A]] =
    endomorphicMonoid[Kleisli[F, *, *], A]


  //  todo  实现
  //   implicit def cokleisliEndoInstance[F[_]: Comonad, A]: Monoid[Endomorphic[Cokleisli[F, *, *], A]] =
  //    endomorphicMonoid[Cokleisli[F, *, *], A]
}

sealed abstract class EndomorphicInstances0 extends EndomorphicInstances1 {

  //Monoid 从寓意上 类似Category
  implicit def endomorphicMonoid[=>:[_, _], A](implicit G: Category[=>:]): Monoid[Endomorphic[=>:, A]] =
    new Monoid[Endomorphic[=>:, A]] with EndomorphicSemigroup[=>:, A] {
      val F: Compose[=>:] = G

      def zero: Endomorphic[=>:, A] = Endomorphic(G.id)
    }
}


sealed abstract class EndomorphicInstances1 extends EndomorphicInstances2 {

  implicit def kleisliEndoSemigroup[F[_] : Bind, A]: Semigroup[Endomorphic[Kleisli[F, *, *], A]] =
    endomorphicSemigroup[Kleisli[F, *, *], A]

  implicit def cokleisliEndoSemigroup[F[_] : Cobind, A]: Semigroup[Endomorphic[Cokleisli[F, *, *], A]] =
    endomorphicSemigroup[Cokleisli[F, *, *], A]

}


sealed abstract class EndomorphicInstances2 {
  // Compose 从寓意上 类似Semigroup
  implicit def endomorphicSemigroup[=>:[_, _], A](implicit G: Compose[=>:]): Semigroup[Endomorphic[=>:, A]] =
    new EndomorphicSemigroup[=>:, A] {
      val F: Compose[=>:] = G
    }
}


private trait EndomorphicSemigroup[=>:[_, _], A] extends Semigroup[Endomorphic[=>:, A]] {

  implicit def F: Compose[=>:]

  def append(f1: Endomorphic[=>:, A], f2: => Endomorphic[=>:, A]): Endomorphic[=>:, A] =
    Endomorphic(F.compose(f1.run, f2.run))
}