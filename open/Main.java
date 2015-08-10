import java.util.*;
import java.util.Random;
public class Main {
    public static double _took(long t0) {
        return (System.nanoTime() - t0) / 1000000D;
    }
    public static void main(String[] args) {
        for (int ii = 0; ii < 1000; ii++) {
            System.gc();
            LongLongMap m = new LongLongMap(128);
            Map<Long,Long> mm = new HashMap<Long,Long>(128);
            long key = 19;
            int n = 20;
            int bump = args.length > 0 ? Integer.parseInt(args[0]) : 10000000;
            long t0 = System.nanoTime();
            Random r = new Random();
            for (int i = 0; i < n; i++) {
                m.incrementOrSet(i,i); // or m.put(i,i);
                long rand = r.nextLong();
                // m.put(rand,rand); // or m.put(i,i);
                // if (m.get(rand) != rand)
                //     throw new RuntimeException("nop, " + m.get(rand) + " != " + rand);
            }
            double took = _took(t0);
            System.out.println("put took: " + took);

            int sum = 0;
            for (int j = 1; j < 10; j++) {
                t0 = System.nanoTime();
                m.get_collisions = 0;
                for (int i = 0; i < (bump * j); i++) {
                    sum += m.get(key);
                }
                took = _took(t0);
                System.out.println("get took: " + took + " for " + (bump*j) + " get collisions: " + m.get_collisions);
            }

            t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                mm.put((long)i,10000L);
                long rand = r.nextLong();
                mm.put(rand,rand); // or m.put(i,i);
            }
            took = _took(t0);
            System.out.println("mm put took: " + took);

            for (int j = 1; j < 10; j++) {
                t0 = System.nanoTime();
                for (int i = 0; i < (bump * j); i++) {
                    sum += mm.get((long)key);
                }
                took = _took(t0);
                System.out.println("mm get took: " + took + " for " + (bump*j));
            }
        }
    }
}
