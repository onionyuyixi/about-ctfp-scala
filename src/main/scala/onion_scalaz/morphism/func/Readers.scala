package onion_scalaz.morphism.func

import onion_scalaz.{Id, Reader, ReaderT}
import onion_scalaz.morphism.Kleisli


object ReaderT {

  def apply[F[_], E, A](f: E => F[A]): ReaderT[E, F, A] = Kleisli[F, E, A](f)

}

object Reader {

  def apply[E, A](f: E => A): Reader[E, A] = Kleisli[Id, E, A](f)

}



