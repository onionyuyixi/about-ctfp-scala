

package onion_scalaz


trait Compose[=>:[_, _]] {

  self =>

  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C


  protected[this] trait ComposeSemigroup[A] extends SemiGroup[A =>: A] {
    override def append(f1: A =>: A, f2: => A =>: A): A =>: A = self.compose(f1, f2)
  }

}
