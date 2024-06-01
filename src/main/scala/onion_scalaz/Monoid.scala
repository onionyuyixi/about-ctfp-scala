package onion_scalaz

trait Monoid[F] extends Semigroup[F] {

  def zero: F


}
