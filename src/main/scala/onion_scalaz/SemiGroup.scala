
package onion_scalaz

trait SemiGroup[F] {

  def append(f1: F, f2: => F): F

}
