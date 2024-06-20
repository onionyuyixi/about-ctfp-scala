package onion_scalaz.morphism.functionusage

import onion_scalaz.{Id, WriterT}
import scalaz.WriterT


object Writer {

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[W, Id, A] = WriterT[W, Id, A]((w, a))
  }


}
