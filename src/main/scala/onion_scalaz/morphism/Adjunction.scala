package onion_scalaz.morphism

import onion_scalaz.morphism.functors.Functor

//𝐿 = 𝐿 ∘ 𝐼𝐃 → 𝐿 ∘ 𝑅 ∘ 𝐿 → 𝐼𝐂 ∘ 𝐿 = 𝐿
//𝑅 = 𝐼𝐃 ∘ 𝑅 → 𝑅 ∘ 𝐿 ∘ 𝑅 → 𝑅 ∘ 𝐼𝐂 = R

abstract class Adjunction[F[_], G[_]](implicit val F: Functor[F], val G: Functor[G]) {
  self =>

  // lift a to the functor that G compose F
  // but G compose F is the Id for type A
  def unit[A](a: => A): G[F[A]] = {
    val func: (F[A] => F[A]) => G[F[A]] = leftAdjunct(a)
    func(identity)
  }

  // extract a from functor that F compose G
  def counit[A](a: F[G[A]]): A = {
    val func: (G[A] => G[A]) => A = rightAdjunct(a)
    func(identity)
  }


  def leftAdjunct[A, B](a: => A)(f: F[A] => B): G[B] =
    G.map(unit(a))(f)

  def rightAdjunct[A, B](a: F[A])(f: A => G[B]): B =
    counit(F.map(a)(f))


}