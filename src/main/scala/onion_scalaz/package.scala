package object onion_scalaz {

  type Id[X] = X

  type <~<[-A, +B] = Liskov[A, B]

  type >~>[+B, -A] = Liskov[A, B]

  type ===[A, B] = Leibniz[Nothing, Any, A, B]




}
