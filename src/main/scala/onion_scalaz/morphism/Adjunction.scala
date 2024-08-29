package onion_scalaz.morphism

import onion_scalaz.morphism.functors.Functor

//𝐿 = 𝐿 ∘ 𝐼𝐃 → 𝐿 ∘ 𝑅 ∘ 𝐿 → 𝐼𝐂 ∘ 𝐿 = 𝐿
//𝑅 = 𝐼𝐃 ∘ 𝑅 → 𝑅 ∘ 𝐿 ∘ 𝑅 → 𝑅 ∘ 𝐼𝐂 = R
//every monad or comonad may be factorized
//into a pair of adjoint functors — this factorization is not unique, though
abstract class Adjunction[F[_], G[_]](implicit val F: Functor[F], val G: Functor[G]) {
  self =>

  // lift a to the functor that G compose F
  // but G compose F is the Id for type A
  // 𝐃(𝐼𝑑,(𝑅 ∘ 𝐿)𝑑)
  def unit[A](a: => A): G[F[A]] = {
    val func: (F[A] => F[A]) => G[F[A]] = leftAdjunct(a)
    func(identity)
  }

  // extract a from functor that F compose G
  def counit[A](a: F[G[A]]): A = {
    val func: (G[A] => G[A]) => A = rightAdjunct(a)
    func(identity)
  }

  //𝐂(𝐿𝑑, 𝑐) ≅ 𝐃(𝑑, 𝑅𝑐)
  // Every F-algebra maps to a G-coalgebra.
  // F-algebra    𝐂(𝐿𝑑, 𝑐)
  // G-coalgebra. 𝐃(𝑑, 𝑅𝑐)
  def leftAdjunct[A, B](a: => A)(f: F[A] => B): G[B] =
    G.map(unit(a))(f)

  def rightAdjunct[A, B](a: F[A])(f: A => G[B]): B =
    counit(F.map(a)(f))


}