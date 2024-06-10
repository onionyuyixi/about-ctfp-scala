package onion_scalaz.morphism.functionusage


object Writer {

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[W, Id, A] = WriterT[W, Id, A]((w, a))
  }


}
