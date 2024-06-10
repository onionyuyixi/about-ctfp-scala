package onion_scalaz.morphism

import onion_scalaz.{<~~>, Id}

// type parameter 有两个参数
trait BiFunctor[F[_, _]] {

  self =>

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def widen[A, B, C >: A, D >: B](fab: F[A, B]): F[C, D] =
    bimap(fab)(identity[C], identity[D])

  def compose[G[_, _]](implicit G0: BiFunctor[G]): BiFunctor[λ[(a, b) => F[G[a, b], G[a, b]]]] =
    new BiFunctorComposition[F, G] {
      override implicit def F: BiFunctor[F] = self

      override implicit def G: BiFunctor[G] = G0
    }


  def leftFunctor[X]: Functor[F[*, X]] =
    new LeftFunctor[F, X] {
      val F: BiFunctor[F] = self
    }

  def rightFunctor[X]: Functor[F[X, *]] =
    new RightFunctor[F, X] {
      val F: BiFunctor[F] = self
    }

  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = {
    val func: (A => C) => F[C, B] = leftFunctor.map(fab)
    func(f)
  }

  def rightMap[A, B, D](fab: F[A, B])(g: B => D): F[A, D] =
    rightFunctor(fab)(g)

  def umap[A, B](faa: F[A, A])(f: A => B): F[B, B] =
    bimap(faa)(f, f)


  def embed[G[_], H[_]](implicit G0: Functor[G], H0: Functor[H]): BiFunctor[λ[(α, β) => F[G[α], H[β]]]] =
    new BiFunctor[λ[(α, β) => F[G[α], H[β]]]] {
      override def bimap[A, B, C, D](fab: F[G[A], H[B]])(f: A => C, g: B => D): F[G[C], H[D]] =
        self.bimap(fab)((ga: G[A]) => G0.map(ga)(f), (hb: H[B]) => H0.map(hb)(g))
    }

  def embedLeft[G[_]](implicit G0: Functor[G]): BiFunctor[λ[(α, β) => F[G[α], β]]] =
    new BiFunctor[λ[(α, β) => F[G[α], β]]] {
      override def bimap[A, B, C, D](fab: F[G[A], B])(f: A => C, g: B => D): F[G[C], D] =
        self.bimap(fab)(ga => G0.map(ga)(f), b => g(b))
    }

  def embedRight[H[_]](implicit H0: Functor[H]): BiFunctor[λ[(α, β) => F[α, H[β]]]] = {
    new BiFunctor[λ[(α, β) => F[α, H[β]]]] {
      override def bimap[A, B, C, D](fab: F[A, H[B]])(f: A => C, g: B => D): F[C, H[D]] =
        self.bimap(fab)(a => f(a), hb => H0.map(hb)(g))
    }

    embed(new Functor[Id] {
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
    }, H0)

  }


  def tupleBiFunctor: BiFunctor[Tuple2] = new BiFunctor[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) = fab match {
      case (a,b)=>(f(a),g(b))
    }
  }




}

object BiFunctor {

  @inline def apply[F[_, _]](implicit F: BiFunctor[F]): BiFunctor[F] = F


  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: BiFunctor[G]): BiFunctor[F] =
    new IsomorphismBifunctor[F, G] {

      override def G: BiFunctor[G] = E

      override def iso: F <~~> G = D
    }


}


trait IsomorphismBifunctor[F[_, _], G[_, _]] extends BiFunctor[F] {

  implicit def iso: F <~~> G


  implicit def G: BiFunctor[G]


  override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    iso.from(G.bimap(iso.to(fab))(f, g))

}