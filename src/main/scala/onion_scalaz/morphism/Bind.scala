package onion_scalaz.morphism

import onion_scalaz.<~>
import onion_scalaz.morphism.functionusage.Kleisli
import scalaz.Equal

trait Bind[F[_]] extends Apply[F] {

  self =>


  // 按较之Apply/Functor中的f  这里的f是跨容器的
  // 所以它这里就不可能是 lift作用
  // bind的意思就是 将本身数据 与容器相 morphism
  // 至于f:A=>F[B] 其意义与Kleisli 差不多
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  def bindFromKleisli[A, B](fa: F[A])(kleisli: Kleisli[F, A, B]): F[B] =
    bind(fa)(kleisli.run)

  def join[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)


  def mproduct[A, B](fa: F[A])(f: Function[A, F[B]]): F[(A, B)] =
    bind(fa)(a => map(f(a))(b => (a, b)))

  trait BindLaw extends ApplyLaw {
    /**
     * As with semigroups, monadic effects only change when their
     * order is changed, not when the order in which they're
     * combined changes.
     */
    def associativeBind[A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(bind(bind(fa)(f))(g), bind(fa)((a: A) => bind(f(a))(g)))

    /** `ap` is consistent with `bind`. */
    def apLikeDerived[A, B](fa: F[A], f: F[A => B])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(fa)(f), bind(f)(f => map(fa)(f)))
  }

}


object Bind {
  @inline def apply[F[_]](implicit F: Bind[F]): Bind[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Bind[G]): Bind[F] =
    new IsomorphismBind[F, G] {
      override def G: Bind[G] = E

      override def iso: F <~> G = D
    }


}

trait IsomorphismBind[F[_], G[_]] extends Bind[F] with IsomorphismApply[F, G] {
  implicit def G: Bind[G]

  override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
    iso.from(G.bind(iso.to(fa))(f.andThen(iso.to.apply)))
}