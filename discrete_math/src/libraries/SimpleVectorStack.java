package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleVectorStack {
    SimpleVector vector = new SimpleVector();
    int size = 0;

    public void push(Object o) {
        vector.set(o, size++);
    }

    public Object pop() {
        if (!this.isEmpty()) {
            ensureCanCompress();
            Object ret = vector.get(--size);
            vector.set(null, size + 1);
            return ret;
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }


    private void ensureCanCompress() {
        if (size * 4 <= vector.size()) {
            vector.compress();
        }
    }

    public int size() {
        return this.size;
    }

    public boolean isEmpty() {
        return size == 0;
    }
}
