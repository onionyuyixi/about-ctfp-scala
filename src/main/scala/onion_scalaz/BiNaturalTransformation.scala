package onion_scalaz

trait BiNaturalTransformation[-F[_, _], +G[_, _]] {

  self =>

  def apply[A, B](fab: F[A, B]): G[A, B]





}
