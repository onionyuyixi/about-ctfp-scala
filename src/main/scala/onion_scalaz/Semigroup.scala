
package onion_scalaz

trait Semigroup[F] {

  def append(f1: F, f2: => F): F






}


trait IsomorphismSemigroup[F, G] extends Semigroup[F] {

  implicit def G: Semigroup[G]

  def iso: F <=> G

  override def append(f1: F, f2: => F): F =
    iso.from(G.append(iso.to(f1), iso.to(f2)))
}