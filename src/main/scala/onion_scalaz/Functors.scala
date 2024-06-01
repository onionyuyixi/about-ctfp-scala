
package onion_scalaz


trait Functor[F[_]] {

  // lift 函数f 是functor 最基本的定义
  // lift map apply 三个方法 同根同源 可以互相直接转换
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def apply[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

  def map[A, B](fa: F[A])(f: A => B): F[B]


  def strengthL[A, B](a: A, fb: F[B]): F[(A, B)] = map(fb)((a, _))

  def strengthR[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)((_, b))

  def mapply[A, B](a: A)(fab: F[A => B]): F[B] = map(fab)(_(a))

  def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] =
    map(fa)(a => (a, f(a)))

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => Unit)



}


trait BiFunctor[F[_, _]] {

  def map[A, B, C, D](fa: F[A, B]): F[C, D]


}


object Functors {

}
