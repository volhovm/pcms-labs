package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleStack {
    private Object[] elements = new Object[10];
    private int size = 0;

    public void push(Object o) {
        ensureCapacity(size + 1);
        elements[size++] = o;
    }

    private void ensureCapacity(int capacity) {
        if (elements.length < capacity) {
            Object[] e = new Object[capacity * 2];
            System.arraycopy(elements, 0, e, 0, size);
            elements = e;
        }
    }

    public Object pop() {
        assert size > 0; //enabled if -ea
        Object ret = elements[size - 1];
        elements[--size] = null;
        return ret;
    }

    public Object peek() {
        assert size > 0;
        return elements[size - 1];
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return (size == 0);
    }
}
