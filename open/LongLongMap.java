import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.Map;
import java.util.HashMap;
public class LongLongMap {
    public long[] values;
    public long[] keys;
    public boolean has_big_keys;
    public int empty = 0;
    public int get_collisions;
    public static long MISSING = Integer.MAX_VALUE;

    public LongLongMap() { this(32); }
    public LongLongMap(int initial_capacity) {
        if (initial_capacity < 32)
            throw new RuntimeException("initial capacity("+initial_capacity+") < 32");
        initial_capacity = (int) upper_power_of_two(initial_capacity);
        keys = new long[initial_capacity];
        values = new long[initial_capacity];
        Arrays.fill(keys,MISSING);
        empty = initial_capacity;
    }

    public void validate_key(long k) {
        if (k == MISSING)
            throw new RuntimeException("cannot insert key("+k+") = MISSING: " + MISSING);
    }

    public void incrementOrSet(LongLongMap other) {
        for (int i = 0; i < other.keys.length; i++) {
            if (other.keys[i] != MISSING)
                incrementOrSet(other.keys[i],other.values[i]);
        }
    }

    public void incrementOrSet(long k, long inc) {
        validate_key(k);
        long idx = get_stored_index(k);
        if (idx != MISSING) {
            long old = values[(int)idx];
            values[(int)idx] += inc;
        } else {
            if (empty < (keys.length / 2))
                rehash();
            store_new_value(keys,values,k,inc);
            empty--;
        }
    }

    public long put(long k, long v) {
        validate_key(k);

        long idx = get_stored_index(k);
        if (idx != MISSING) {
            long old = values[(int)idx];
            values[(int)idx] = v;
            return old;
        } else {
            if (empty < (keys.length / 2))
                rehash();
            store_new_value(keys,values,k,v);
            empty--;
            return MISSING;
        }
    }

    public void rehash() {
        int len = (int)upper_power_of_two(((int)keys.length * 2) - 1);
        long[] _keys = new long[len];
        long[] _values = new long[len];

        Arrays.fill(_keys,MISSING);
        empty = len;
        for(int i = 0; i < keys.length; i++) {
            if (keys[i] != MISSING) {
                store_new_value(_keys,_values,keys[i],values[i]);
                empty--;
            }
        }

        keys = _keys;
        values = _values;
    }

    public static void store_new_value(long[] _keys, long[] _values, long k, long v) {
        long len = _keys.length;
        long j = 0;
        long idx = hash(k,len);
        while (j < len) {
            if (_keys[(int)idx] == MISSING) {
                _keys[(int)idx] = k;
                _values[(int)idx] = v;
                return;
            }

            j++;
            idx++;
            if (idx == len)
                idx = 0;
        }
        throw new RuntimeException("unable to store in len:" + len);
    }

    public long get_stored_index(long k)  {
        long len = keys.length;
        long j = 0;
        long idx = hash(k,len);
        while (j < len) {
            long item = keys[(int)idx];
            if (item == k)
                return idx;
            if (item == MISSING)
                return MISSING;
            j++;
            idx++;
            if (idx == len)
                idx = 0;
            get_collisions++;
        }
        return MISSING;
    }

    public long get(long k) {
        long idx = get_stored_index(k);
        return idx != MISSING ? values[(int)idx] : MISSING;
    }

    public static long smear(long hashCode) {
        hashCode ^= (hashCode >>> 20) ^ (hashCode >>> 12);
        return hashCode ^ (hashCode >>> 7) ^ (hashCode >>> 4);
    }

    public static long hash(long x, long length) {
        return (Murmur3.hashLong(x) & (length - 1));
    }

    public void forEach(BiConsumer<Long, Long> consumer) {
        for(int i = 0; i < keys.length; i++) {
            if (keys[i] != MISSING)
                consumer.accept(keys[i], values[i]);
        }
    }

    public Map<Long,Long> as_map() {
        Map<Long,Long> m = new HashMap<Long,Long>();
        forEach((k,n) -> m.put(k,n));
        return m;
    }

    long upper_power_of_two(long v) {
        v--;
        v |= v >> 1;
        v |= v >> 2;
        v |= v >> 4;
        v |= v >> 8;
        v |= v >> 16;
        v++;
        return v;
    }
}
