package onion_scalaz.category_base

import onion_scalaz.<~~>
import onion_scalaz.morphism.Applicative


trait Arrow[=>:[_, _]] extends Split[=>:] with Strong[=>:] with Category[=>:] {

  self =>

  // lift morphism f
  def arr[A, B](f: A => B): A =>: B

  // 就是compose
  final def <<<[A, B, C](fbc: B =>: C, fab: A =>: B): =>:[A, C] =
    compose(fbc, fab)

  // 相当于 andThen
  def >>>[A, B, C](fab: A =>: B, fbc: B =>: C): (A =>: C) =
    compose(fbc, fab)

  def combine[A, B, C](fab: A =>: B, fac: A =>: C): A =>: (B, C) = {

    val func1: (A, A) =>: (B, C) = split(fab, fac)

    val func2: A =>: (A, A) = arr((a: A) => (a, a))

    >>>(func2, func1)

    >>>(arr((a: A) => (a, a)), split(fab, fac))
  }


  def swap[X, Y]: (X, Y) =>: (Y, X) = arr[(X, Y), (Y, X)] {
    case (x, y) => (y, x)
  }

  override def second[A, B, C](f: A =>: B): (C, A) =>: (C, B) = {
    val func1: (C, A) =>: (B, C) = <<<(first[A, B, C](f), swap[C, A])
    >>>(func1, swap[B, C])
  }

  final def splitA[A, B, C, D](fab: (A =>: B), fcd: (C =>: D)): ((A, C) =>: (B, D)) =
    split(fab, fcd)

  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A, C) =>: (B, D) = {
    // 各自映射 再结合起来
    val f1: (A, C) =>: (B, C) = first(f)

    val f2: (B, C) =>: (B, D) = second(g)

    >>>(f1, f2)
  }


  def product[A, B](fab: A =>: B): (A, A) =>: (B, B) =
    splitA(fab, fab)

  def mapfst[A, B, C](fab: A =>: B)(f: C => A): C =>: B =
    >>>(arr(f), fab)

  def mapsnd[A, B, C](fab: A =>: B)(f: B => C): A =>: C =
    >>>(fab, arr(f))


  override def covariantInstance[C]: Applicative[=>:[C, *]] =
    new Applicative[=>:[C, *]] with SndCovariant[C] {

      def point[A](a: => A): C =>: A = arr(_ => a)

      def ap[A, B](fa: => C =>: A)(f: => C =>: (A => B)): C =>: B =
        self.compose(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
    }

}

object Arrow {
  @inline def apply[F[_, _]](implicit F: Arrow[F]): Arrow[F] = F


  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Arrow[G]): Arrow[F] =
    new IsomorphismArrow[F, G] {
      override implicit def G: Arrow[G] = E

      override def iso: F <~~> G = D
    }

}


trait IsomorphismArrow[F[_, _], G[_, _]] extends Arrow[F] with IsomorphismSplit[F, G]
  with IsomorphismStrong[F, G] with IsomorphismCategory[F, G] {

  implicit def G: Arrow[G]

  override def arr[A, B](f: A => B): F[A, B] =
    iso.from(G.arr(f))

}
