package partone

trait Monoid[A] {
  def identity: A

  def combine(x: A, y: A): A

  def compos(f: A => A, g: A => A): A => A = x => (f compose g)(x)
}

object Monoid {
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def identity: String = ""

    def combine(x: String, y: String): String = x + y
  }

  implicit val intAddMonoid: Monoid[Int] = new Monoid[Int] {
    def identity: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val intMulMonoid: Monoid[Int] = new Monoid[Int] {
    def identity: Int = 1

    def combine(x: Int, y: Int): Int = x * y
  }

}

object MitageSideEffect {

  var log: String = ""

  def negateSideEffect(boolean: Boolean): Boolean = {
    log += "Not so"
    !boolean
  }

  def negateNoEffect(boolean: Boolean, string: String): (Boolean, String) =
    (!boolean, string + "Not so")

  def negateNoEffect1(boolean: Boolean): (Boolean, String) =
    (!boolean, "Not so")

  type Writer[A] = (String, A)


  object Kleisli {
    implicit class KleisliOps[A, B](f: A => Writer[B]) {
      def >=>[C](f: A => Writer[B], g: B => Writer[C]): A => Writer[C] = a => {
        val (log1, b) = f(a)
        val (log2, c) = g(b)
        (log1 + log2, c)
      }
    }
  }


}




