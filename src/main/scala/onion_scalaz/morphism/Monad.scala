package onion_scalaz.morphism

import onion_scalaz.<~>
import onion_scalaz.monoid.Monoid
import onion_scalaz.morphism.functors.{Functor, ProductMonad}
import scalaz.{Equal, Need}


// 结合了 Applicative 的lift and point  Bind的跨容器绑定
trait Monad[F[_]] extends Applicative[F] with Bind[F] {


  self =>


  // 这里主要阐释了 lift的作用就是 通过point 和 bind 来实现的
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    bind(fa)(a => point(f(a)))

  // ap 可以用bind 和 map 实现
  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = {
    bind(f)(x => map(fa)(x))
  }

  def product[G[_]](implicit G0: Monad[G]): Monad[λ[α => (F[α], G[α])]] =
    new ProductMonad[F, G] {
      def F: Monad[F] = self

      def G: Monad[G] = G0
    }

  trait MonadLaw extends ApplicativeLaw with BindLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)

    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
  }


}


object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F


  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Monad[G]): Bind[F] =
    new IsomorphismMonad[F, G] {
      override def G: Monad[G] = E

      override def iso: F <~> G = D
    }

  // 整体表达的是 A=>Writer[W,B]  embellished functions
  // final case class Kleisli[M[_], A, B](run: A => M[B]) 这二者表达的语义差不多
  // refers to the p346
  implicit def writerFunctor[W: Monoid]: Functor[Writer[W, *]] = new Monad[Writer[W, *]] {

    override def point[A](a: => A): Writer[W, A] =
      Writer((a, Monoid[W].zero))

    override def bind[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = fa match {
      case Writer((a, aw)) =>
        val (b, bw) = f(a)
        Writer(b, Monoid[W].append(aw, bw))
    }

    override def ap[A, B](fa: => Writer[W, A])(f: => Writer[W, A => B]): Writer[W, B] =
      fa match {
        case Writer((a, aw)) =>
          f match {
            case Writer((fab, fw)) =>
              Writer(fab(a), Monoid[W].append(aw, fw))
          }
      }
  }

  implicit def readerFunctor[E]: Functor[Reader[E, *]] = new Monad[Reader[E, *]] {
    override def point[A](a: => A): Reader[E, A] =
      Reader(_ => a)

    override def bind[A, B](fa: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
      Reader {
        e => runReader(f(runReader(fa)(e)))(e)
      }

    override def ap[A, B](fa: => Reader[E, A])(f: => Reader[E, A => B]): Reader[E, B] =
      Reader {
        e =>
          val a: A = runReader(fa)(e)
          val b: B = runReader(f)(e)(a)
          b
      }
  }

  implicit def stateFunctor[S]: Functor[State[S, *]] = new Monad[State[S, *]] {
    override def point[A](a: => A): State[S, A] =
      State((a, _))

    override def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State {
        s =>
          val (a, sa) = runState(fa)(s)
          runState(f(a))(sa)
      }

    override def ap[A, B](fa: => State[S, A])(f: => State[S, A => B]): State[S, B] =
      State {
        s =>
          val (a, sa) = runState(fa)(s)
          val b = runState(f)(s)._1(a)
          (b, sa)
      }
  }

  implicit def contFunctor[R]: Functor[Cont[R, *]] = new Monad[Cont[R, *]] {
    override def point[A](a: => A): Cont[R, A] = Cont(f => f(a))

    override def bind[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] =
      Cont {
        func =>
          val fst: (A => R) => R = runCont(fa)
          fst(a => runCont(f(a))(func))
      }
  }

  def runReader[E, A]: Reader[E, A] => E => A = {
    case Reader(run) =>
      e => run(e)
  }

  def runWriter[W, A]: Writer[W, A] => (A, W) = {
    case Writer((a, w)) => (a, w)
  }

  def runState[S, A]: State[S, A] => S => (A, S) = {
    case State(run) => s => run(s)
  }

  def runCont[R, A]: Cont[R, A] => (A => R) => R = {
    case Cont(run) => f => run(f)
  }

}


case class Writer[W, A](run: (A, W))

case class Reader[E, A](run: E => A)

case class State[S, A](run: S => (A, S))

case class Cont[R, A](run: (A => R) => R)

trait IsomorphismMonad[F[_], G[_]] extends Monad[F] with IsomorphismApplicative[F, G] with IsomorphismBind[F, G] {
  override implicit def G: Monad[G]

}