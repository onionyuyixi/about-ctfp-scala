package partone.java;

import java.util.function.Function;

public record JavaBiFunctor<A, B>(A a, B b) {

    private record BiContainer<A, B>(Function0<A> f0a, Function0<B> f0b) {

        public <C, D> BiContainer<C, D> bimap0(Function<Function0<A>, C> fac, Function<Function0<B>, D> fbd) {
            return new BiContainer<>(() -> fac.apply(this.f0a), () -> fbd.apply(this.f0b));
        }

        public <C, D> BiContainer<C, D> bimap(Function<A, C> fac, Function<B, D> fbd) {
            return new BiContainer<>(() -> fac.apply(this.f0a.eval()), () -> fbd.apply(this.f0b.eval()));
        }

    }

    public BiContainer<A, B> biunit(A a, B b) {
        return new BiContainer<>(() -> a, () -> b);
    }

    public <C, D> BiContainer<C, D> bimap(Function<A, C> fac, Function<B, D> fbd) {

        return new BiContainer<>(() -> fac.apply(this.a), () -> fbd.apply(this.b));

    }


}
