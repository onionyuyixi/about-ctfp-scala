package partthree

import partthree.Algebra.{Algebra, cata}
import partthree.Fix.unFix
import scalaz.{Functor, Monad}

sealed trait MonF[+A]

case object MEmpty extends MonF[Nothing]

case class MAppend[+A](m: A, n: A) extends MonF[A]


sealed trait RingF[+A]

case object RZero extends RingF[Nothing]

case object ROne extends RingF[Nothing]

case class RAdd[A](m: A, n: A) extends RingF[A]

case class RMul[A](m: A, n: A) extends RingF[A]

case class RNeg[A](n: A) extends RingF[A]

sealed trait Expr

case object Zero extends Expr

case object One extends Expr

case class Add(e1: Expr, e2: Expr) extends Expr

case class Mul(e1: Expr, e2: Expr) extends Expr

case class Neg(e: Expr) extends Expr

// 不停地在F上嵌套 类似tree的结构
// Applying an endofunctor infinitely many times produces a fixed point,
// an object defined as: 𝐹𝑖𝑥 𝑓 = 𝑓 (𝐹𝑖𝑥 𝑓 )
// The intuition behind this definition is that, since we applied 𝑓 infinitely
// many times to get 𝐹𝑖𝑥 𝑓 , applying it one more time doesn’t change anything
// f 就是类似Expr的ADT structure data
// algebras over a given endofunctor 𝐹 form a category  可以参考Endo
case class Fix[F[_]](x: F[Fix[F]])

object Fix {

  // p437 减少了在F上的嵌套
  def apply[F[_]](f: F[Fix[F]]): Fix[F] = new Fix(f)

  // 增加了在F上的嵌套
  def unFix[F[_]]: Fix[F] => F[Fix[F]] = {
    case Fix(x) => x
  }

}


object Algebra {

  // p426 in ctfp-scala
  // m × m + m + 1 → m
  // An F-algebra is a triple consisting of an endofunctor 𝐹, an object 𝑎, and a morphism Fa -> a
  // The object 𝑎 is often called the carrier, an underlying object or, in the context of programming, the carrier type.
  // The morphism is often called the evaluation function or the structure map
  type Algebra[F[_], A] = F[A] => A

  // the role of the functor is to generate expressions that can be evaluated using the evaluator of the algebra
  def evalZ: Algebra[RingF, Int] = {
    case RZero => 0
    case ROne => 1
    case RAdd(m, n) => m + n
    case RMul(m, n) => m * n
    case RNeg(n) => -n
  }

  def evalZ1: Expr => Int = {
    case Zero => 0
    case One => 1
    case Add(e1, e2) => evalZ1(e1) + evalZ1(e2)
    case Mul(e1, e2) => evalZ1(e1) * evalZ1(e2)
    case Neg(e) => -evalZ1(e)
  }

  type RingF1[A] = RingF[RingF[A]]

  //type RingF2[A] = RingF[RingF[RingF[A]]]
  type RingF2[A] = RingF[RingF1[A]]


  def cata[F[_], A](alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {


    // 递归节点
    val func1: F[Fix[F]] => F[A] = F.lift(cata(alg))

    // 将所有节点转变成F[_]容器中数据
    val func2: Fix[F] => F[A] = func1 compose unFix

    // 计算结果
    alg.compose(func2)

  }

  def ana[F[_], A](coalg: A => F[A])(implicit F: Functor[F]): A => Fix[F] = {

    val func1: F[A] => F[Fix[F]] = F.lift(ana(coalg))

    val func2: A => F[Fix[F]] = func1 compose coalg

    Fix.apply[F] compose func2
  }


}


object TestAlgebra extends App {

  sealed trait NatF[+A]

  case object ZeroF extends NatF[Nothing]

  case class SuccF[A](a: A) extends NatF[A]


  // algebra type
  def fib: Algebra[NatF, (Int, Int)] = {
    case ZeroF => (1, 1)
    case SuccF((m, n)) => (n, m + n)
  }

  implicit def nafFunctor: Functor[NatF] = new Functor[NatF] {
    override def map[A, B](fa: NatF[A])(f: A => B): NatF[B] =
      fa match {
        case ZeroF => ZeroF
        case SuccF(a) => SuccF(f(a))
      }
  }

  private val func: Fix[NatF] => (Int, Int) = cata(fib)

  println(func(Fix(SuccF(Fix(ZeroF)))))

}


object TestCoalgebra extends App {

  case class StreamF[E, A](h: E, t: A)

  implicit def streamFFunctor[E]: Functor[StreamF[E, *]] = new Functor[StreamF[E, *]] {
    override def map[A, B](fa: StreamF[E, A])(f: A => B): StreamF[E, B] =
      StreamF(fa.h, f(fa.t))
  }

  case class Stream[E](h: E, t: Stream[E])


}



