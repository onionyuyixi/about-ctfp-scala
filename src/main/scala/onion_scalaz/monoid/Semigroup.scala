
package onion_scalaz

import scalaz.{Equal, Maybe}
import scalaz.Maybe.Just

import scala.annotation.tailrec

// 类似于Compose[=>:[_,_]]
trait Semigroup[F] {

  // 类似Compose[=>:[_,_]] 的compose方法
  def append(f1: F, f2: => F): F


  protected[this] trait SemigroupCompose extends Compose[λ[(a, b) => F]] {
    override def compose[A, B, C](f: F, g: F): F = append(f, g)
  }

  final def compose: Compose[λ[(α, β) => F]] =
    new SemigroupCompose {}


  def unfoldlSumOpt[S](seed: S)(f: S => Maybe[(S, F)]): Maybe[F] =
    defaultUnfoldlSumOpt(seed)(f)

  @inline private def defaultUnfoldlSumOpt[S](seed: S)(f: S => Maybe[(S, F)]): Maybe[F] = {
    @tailrec
    def go(s: S, acc: F): F = f(s) match {
      case Maybe.Just((s, f)) => go(s, append(f, acc))
      case Maybe.Empty() => acc
    }

    f(seed).map {
      case (s, f) => go(s, f)
    }

  }

  def unfoldrSumOpt[S](seed: S)(f: S => Maybe[(F, S)]): Maybe[F] =
    defaultUnfoldrSumOpt(seed)(f)

  @inline private def defaultUnfoldrSumOpt[S](seed: S)(f: S => Maybe[(F, S)]): Maybe[F] = {
    @tailrec def go(acc: F, s: S): F = f(s) match {
      case Just((f, s)) => go(append(acc, f), s)
      case _ => acc
    }

    f(seed) map { case (a, s) => go(a, s) }
  }


  trait SemigroupLaw {
    def associative(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
      F.equal(append(f1, append(f2, f3)), append(append(f1, f2), f3))

    def unfoldlSumOptConsistency[S](s: S, f: S => Maybe[(S, F)])(implicit E: Equal[F]): Boolean = {
      val g: ((Int, S)) => Maybe[((Int, S), F)] = {
        case (i, s) =>
          if (i > 0) f(s) map { case (s, f) => ((i - 1, s), f) }
          else Maybe.empty
      }
      val limit = 4 // to prevent infinite unfolds
      Equal[Maybe[F]].equal(unfoldlSumOpt((limit, s))(g), defaultUnfoldlSumOpt((limit, s))(g))
    }

    def unfoldrSumOptConsistency[S](s: S, f: S => Maybe[(F, S)])(implicit E: Equal[F]): Boolean = {
      val g: ((Int, S)) => Maybe[(F, (Int, S))] = {
        case (i, s) =>
          if (i > 0) f(s) map { case (f, s) => (f, (i - 1, s)) }
          else Maybe.empty
      }
      val limit = 4 // to prevent infinite unfolds
      Equal[Maybe[F]].equal(unfoldrSumOpt((limit, s))(g), defaultUnfoldrSumOpt((limit, s))(g))
    }
  }

}


trait IsomorphismSemigroup[F, G] extends Semigroup[F] {

  implicit def G: Semigroup[G]

  def iso: F <=> G

  override def append(f1: F, f2: => F): F =
    iso.from(G.append(iso.to(f1), iso.to(f2)))
}