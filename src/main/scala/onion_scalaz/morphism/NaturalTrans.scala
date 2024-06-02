
package onion_scalaz.morphism

trait NaturalTrans[F[_], G[_]] {

  self =>

  def apply[A](fa: F[A]): G[A]

}

trait BiNaturalTrans[F[_,_], G[_,_]] {

  self =>

  def apply[A,B](fa: F[A,B]): G[A,B]

}

