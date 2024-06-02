import onion_scalaz.morphism.BijectionT

package object onion_scalaz {

  type Id[X] = X

  type <~<[-A, +B] = Liskov[A, B]

  type >~>[+B, -A] = Liskov[A, B]

  type ===[A, B] = Leibniz[Nothing, Any, A, B]

  type IsoSet[A, B] = Iso[Function1, A, B]

  type <=>[A, B] = IsoSet[A, B]

  type IsoFunctor[F[_], G[_]] = FunctorIso[NaturalTrans, F, G]

  type IsoBifunctor[F[_, _], G[_, _]] = BifunctorIso[BiNaturalTrans, F, G]

  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]

  type <~>[F[_], G[_]] = IsoFunctor[F, G]

  type ~>[F[_], G[_]] = NaturalTrans[F, G]

  type <~[F[_], G[_]] = NaturalTrans[G, F]

  type Bijection[A,B] = BijectionT[Id,Id,A,B]

}
