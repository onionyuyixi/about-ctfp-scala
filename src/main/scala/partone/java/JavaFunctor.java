package partone.java;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


public record JavaFunctor<A>(Function0<A> a) {

    private record Container<A>(Function0<A> a) {

        public <B> Container<B> map0(Function<Function0<A>, Function0<B>> f) {
            return new Container<>(f.apply(this.a));
        }


        public <B> Container<B> map(Function<A, B> f) {
            return new Container<>(() -> f.apply(this.a.eval()));
        }

        public static <A1> Container<A1> id(A1 a) {
            return new Container<>(() -> a);
        }

    }


    public static <A> Container<A> unit(A a) {
        return new Container<>(() -> a);
    }

    public <B> Container<B> map(Function<A, B> f) {
        return new Container<>(() -> f.apply(this.a.eval()));
    }


    public static void main(String[] args) {

        List<String> strs = IntStream.range(1, 100).mapToObj(String::valueOf).toList();
        int i1 = 10000 * 100;

        int itr = 1;
        new Thread(() -> {
            List<Long> lazy = new ArrayList<>(itr * i1);
            for (int n = 0; n < itr; n++) {

                long l1 = System.currentTimeMillis();
                for (int i = 0; i < i1; i++) {
                    JavaFunctor.unit(i)
                            .map((String::valueOf))
                            .map0(strF0 -> strF0.map(f0 -> () -> f0.eval() + strs.parallelStream().collect(Collectors.joining())))
                            .a.eval();
                }
                lazy.add(System.currentTimeMillis() - l1);

            }

            String x = "lazy sum -> " + lazy.stream().reduce(Long::sum).orElse(0L);
            System.err.println(x);
        }).start();

        new Thread(() -> {

            List<Long> noLazy = new ArrayList<>(itr * i1);
            for (int n = 0; n < itr; n++) {

                long l = System.currentTimeMillis();
                for (int i = 0; i < i1; i++) {
                    JavaFunctor.unit(i)
                            .map(String::valueOf)
                            .map(a -> a + strs.parallelStream().collect(Collectors.joining()))
                            .a.eval();
                }

                noLazy.add(System.currentTimeMillis() - l);

            }

            String x1 = "no lazy sum -> " + noLazy.stream().reduce(Long::sum).orElse(0L);
            System.err.println(x1);
        }).start();


        List<Integer> integers = List.of(1, 2, 3);
        System.err.println(Container.id(integers));
        System.err.println(Container.id(integers).a().eval());

    }
}



