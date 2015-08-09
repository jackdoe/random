import java.util.*;
public class Main {
    public static double _took(long t0) {
        return (System.nanoTime() - t0) / 1000000D;
    }
    public static void main(String[] args) {
        for (int ii = 0; ii < 10; ii++) {
            System.gc();
            IntIntMap m = new IntIntMap(100);
            Map<Integer,Integer> mm = new HashMap<Integer,Integer>(100);
            int n = 20000;
            int bump = args.length > 0 ? Integer.parseInt(args[0]) : 10000000;
            long t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                m.put(i,i);
            }
            double took = _took(t0);
            System.out.println("put took: " + took);
            t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                if (m.get(i) != i)
                    throw new RuntimeException("i("+i+") = ("+m.get(i)+")");
            }
            took = _took(t0);
            System.out.println("get took: " + took);

            for (int j = 1; j < 10; j++) {
                t0 = System.nanoTime();
                for (int i = 0; i < (bump * j); i++) {
                    if (m.get(199) != 199)
                        throw new RuntimeException("i("+i+") = ("+m.get(i)+")");
                }
                took = _took(t0);
                System.out.println("get took: " + took + " for " + (bump*j));
            }

            t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                mm.put(i,10000);
            }
            took = _took(t0);
            System.out.println("mm put took: " + took);

            t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                if (mm.get(i) != 10000)
                    throw new RuntimeException("i("+i+") != 10000("+m.get(i)+")");
            }
            took = _took(t0);
            System.out.println("mm get took: " + took);

            for (int j = 1; j < 10; j++) {
                t0 = System.nanoTime();
                for (int i = 0; i < (bump * j); i++) {
                    if (mm.get(199) != 10000)
                        throw new RuntimeException("i("+i+") != 10000("+m.get(i)+")");
                }
                took = _took(t0);
                System.out.println("mm get took: " + took + " for " + (bump*j));
            }
        }
    }
}
