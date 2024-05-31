package partone


case class Writer[A](a: A, str: String) {

}


object Kleisli {

  implicit class KleisliOps[A, B](f: A => Writer[B]) {
    def >=>[C](g: B => Writer[C]): A => Writer[C] = a => {
      val wb = f(a)
      val wc = g(wb.a)
      Writer(wc.a, wb.str + wc.str)
    }
  }

}


object TestKleisli extends App {


}

