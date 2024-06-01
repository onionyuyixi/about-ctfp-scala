package onion_scalaz

// 用来表达 交换律
trait Associative[==>:[_, _]] {

  self =>

  def reassociateLeft[A, B, C](f: A ==>: (B ==>: C)): (A ==>: B) ==>: C

  def reassociateRight[A, B, C](f: (A ==>: B) ==>: C): A ==>: (B ==>: C)

  def reassociateIso[A, B, C]: ((A ==>: B) ==>: C) <=> (A ==>: (B ==>: C)) = IsoSet(reassociateRight, reassociateLeft)

}


object Associative {

  @inline def apply[F[_, _]](implicit fa: Associative[F]): Associative[F] = fa


}