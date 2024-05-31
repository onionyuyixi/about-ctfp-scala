package partone.java;

import java.util.function.Function;

public record JavaReader<C, A>(Function<C, A> run) {


    public <B> JavaReader<C, B> map(Function<A, B> f) {
        Function<C, B> fcb = this.run.andThen(f);
        return new JavaReader<>(fcb);
    }

}
