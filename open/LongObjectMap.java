import java.util.Arrays;
public class LongObjectMap {
    public Object[] values;
    public long[] keys;
    public int empty = 0;

    public static int MISSING = Integer.MAX_VALUE;

    public LongObjectMap() { this(10); }
    public LongObjectMap(int initial_capacity) {
        if (initial_capacity < 5)
            throw new RuntimeException("initial capacity("+initial_capacity+") < 5");

        keys = new long[initial_capacity];
        values = new Object[initial_capacity];
        Arrays.fill(keys,MISSING);
        empty = initial_capacity;
    }

    public Object put(long k, Object v) {
        if (k == MISSING)
            throw new RuntimeException("cannot insert key("+k+") = MISSING: " + MISSING);

        int idx = get_stored_index(k);
        if (idx != MISSING) {
            Object old = values[idx];
            values[idx] = v;
            return old;
        } else {
            if (empty < (keys.length / 2))
                rehash();
            store_new_value(keys,values,k,v);
            empty--;
            return null;
        }
    }

    public void rehash() {
        int len = keys.length * 2;
        long[] _keys = new long[len];
        Object[] _values = new Object[len];

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

    public static void store_new_value(long[] _keys, Object[] _values,long k, Object v) {
        int len = _keys.length;
        int j = hash(k,len);
        int idx = j;
        while (j < len) {
            if (_keys[idx] == MISSING) {
                _keys[idx] = k;
                _values[idx] = v;
                return;
            }
            j++;
            idx = j % len;
        }
        throw new RuntimeException("unable to store in len:" + len);
    }

    public int get_stored_index(long k)  {
        int len = keys.length;
        int j = hash(k,len);
        int idx = j;
        while (j < len) {
            long item = keys[idx];
            if (item == k)
                return idx;
            if (item == MISSING)
                return MISSING;
            j++;
            idx = j % len;
        }
        return MISSING;
    }

    public Object get(long k) {
        int idx = get_stored_index(k);
        return idx != MISSING ? values[idx] : null;
    }

    public static int smear(int hashCode) {
        hashCode ^= (hashCode >>> 20) ^ (hashCode >>> 12);
        return hashCode ^ (hashCode >>> 7) ^ (hashCode >>> 4);
    }

    public static int hash(long x, int length) {
        x ^= (x >>> 32);
        return smear((int)x) % length;
    }
}
