package onion_scalaz.morphism


// 双端映射 但涉及了两个Functor F G  必然需要Natural Trans
// 当F G都是Id的时候 就获得的满射 A=>B B=>A
final case class BijectionT[F[_], G[_], A, B](_to: A => F[B], _from: B => G[A]) {


  def to(a: A): F[B] = _to(a)

  def from(b: B): G[A] = _from(b)

  def flip: BijectionT[G, F, B, A] = BijectionT(_from, _to)


  def toK: Kleisli[F, A, B] = Kleisli(_to)

  def fromK: Kleisli[G, B, A] = Kleisli(_from)

}
