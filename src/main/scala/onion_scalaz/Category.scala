
package onion_scalaz

import scalaz.{Equal, IsomorphismCompose}

//
trait Category[=>:[_, _]] extends Compose[=>:] {

  def id[A]: A =>: A


  def monoid[A]: Monoid[A =>: A] = new Monoid[A =>: A] with ComposeSemigroup[A] {

    override def zero: A =>: A = id
  }


  trait CategoryLaw extends ComposeLaw {

    // category 还要强调 双端运算的相等  这是由zero来进行保障 同样的类型存在Monoid 和 Semigroup中
    def leftIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val v: A =>: B = compose(ab, id[A])
      E.equal(ab, v)
    }

    def rightIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val v: A =>: B = compose(id[A], ab)
      E.equal(ab, v)
    }


  }


}

trait IsoCategory[F[_, _], G[_, _]] extends Category[F] with IsomorphismCompose[F, G] {

  implicit def C: Category[G]

  override def id[A]: F[A, A] = iso.from(C.id)
}

object Category {

  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F


  def fromIso[F[_, _], G[_, _]](D: F <~> G)(implicit E: Category[G]): Category[F] = {
    val from = D.from
    val to = D.to
    new Category[F] {
      override def id[A]: F[A, A] = from(E.id)

      override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
        from(E.compose(to(f), to(g)))
    }
  }


}