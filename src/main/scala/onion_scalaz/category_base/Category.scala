

package onion_scalaz.category_base

import onion_scalaz.monoid.Monoid
import onion_scalaz.morphism.BiNaturalTrans
import onion_scalaz.{<~~>, IsoBifunctor}
import scalaz.Equal

//
trait Category[=>:[_, _]] extends Compose[=>:] {

  def id[A]: A =>: A

  def endoId[A]: Endo[A] = {

    // 好像不得行哇
    val value: IsoBifunctor[Function, =>:] = new IsoBifunctor[Function, =>:] {

      override def to: BiNaturalTrans[Function, =>:] = new BiNaturalTrans[Function, =>:] {
        override def apply[A, B](fa: Function[A, B]): A =>: B = ???
      }

      override def from: BiNaturalTrans[=>:, Function] = new BiNaturalTrans[=>:, Function] {
        override def apply[A, B](fa: A =>: B): Function[A, B] = ???
      }
    }

    val value1: Category[Function] = Category.fromIso[Function, =>:](value)(this)

    Endo(value1.id[A])

  }


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
      val v: A =>: B = compose(id[B], ab)
      E.equal(ab, v)
    }


  }


}

trait IsomorphismCategory[F[_, _], G[_, _]] extends Category[F] with IsomorphismCompose[F, G] {

  implicit def G: Category[G]

  override def id[A]: F[A, A] = iso.from(G.id)
}

object Category {

  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F


  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Category[G]): Category[F] = {
    val from = D.from
    val to = D.to
    new Category[F] {
      override def id[A]: F[A, A] = from(E.id)

      override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
        from(E.compose(to(f), to(g)))
    }
  }


}