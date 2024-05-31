package partone.java;

import java.util.function.Function;


public interface Function0<A> {

    A eval();


    default <B> Function0<B> map(Function<Function0<A>, Function0<B>> fab) {
        return fab.apply(this);
    }


}
