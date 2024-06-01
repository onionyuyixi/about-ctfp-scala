package partone.java;

import java.util.function.Function;

public record Reader<C, A>(Function<C, A> run) {


    public <B> Reader<C, B> map(Function<A, B> f) {
        Function<C, B> fcb = this.run.andThen(f);
        return new Reader<>(fcb);
    }

}
