package partone

import java.util.concurrent.locks.LockSupport

trait Id[A] {
  def id(a: A): A = a
}

object IdObj extends Id[Int] {
  override def id(a: Int): Int = a
}

trait Compos[A, B, C, D] {

  def compos(f: A => B, g: B => C, h: C => D): A => D
  = (a: A) => (h compose g compose f)(a)

  def compos1(f: A => B, g: B => C, h: C => D): A => D
  = (a: A) =>
    ((h compose g) compose f)(a)

  def compos2(f: A => B, g: B => C, h: C => D): A => D
  = (a: A) =>
    (h compose (g compose f))(a)

}

object Compos extends Compos[Int, Int, Int, Int]

object Fact {
  def fact(n: Int): Int = (1 to n).product
}

trait Memo[A, B] {
  def memo(a: A, f: A => B): B
}

object Memo extends Memo[String, Int] {

  var data = Map[String, Int]()

  override def memo(a: String, f: String => Int): Int =
    data.get(a) match {
      case Some(value) =>
        println("right now")
        value
      case None =>
        println("first time to simulate 1 second delay")
        LockSupport.parkNanos(1000*1000*1000)
        val value = f(a)
        data += (a -> value)
        value
    }
}


object test {

  def main(args: Array[String]): Unit = {
    println(IdObj.id(10))

    println(Compos.compos1(x => IdObj.id(x), x => 2 * x, x => x + 1).apply(10))

    println(Fact.fact(5))

    println(Memo.memo("1", a => Integer.valueOf(a)))
    println(Memo.memo("1", a => Integer.valueOf(a)))


  }

}
