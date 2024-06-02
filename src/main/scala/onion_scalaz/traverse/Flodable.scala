package onion_scalaz.traverse

import onion_scalaz.{Monoid, Semigroup}
import scalaz.std.option.some

trait Flodable[F[_]] {
  self =>

  // 这是体现核心的方法 Monoid为其提供zero append
  // 使得计算结果 可以累计起来
  // 两端计算结果都一致
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]):B

  def foldMap1Opt[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] =
    foldMap(fa)(x => some(f(x)))(F)




}



object Flodable {

  @inline def apply[F[_]](implicit F:Flodable[F]): Flodable[F] = F





}