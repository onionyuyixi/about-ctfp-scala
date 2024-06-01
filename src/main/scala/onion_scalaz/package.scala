package object onion_scalaz {

  type Id[X] = X

  type <~<[-A, +B] = Liskov[A, B]

  type >~>[+B, -A] = Liskov[A, B]

  type ===[A, B] = Leibniz[Nothing, Any, A, B]

  type IsoSet[A, B] = Iso[Function1, A, B]

  type <=>[A, B] = IsoSet[A, B]

  type IsoBifunctor[F[_, _], G[_, _]] = BifunctorIso[BiNaturalTransformation, F, G]

  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]


}
