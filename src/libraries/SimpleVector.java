package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleVector {
    private Object[] elements = new Object[10];

    public void set(Object o, int index) {
        ensureCapacity(index);
        elements[index] = o;
    }

    private void ensureCapacity(int capacity) {
        if (elements.length <= capacity) {
            Object[] e = new Object[capacity * 2];
            System.arraycopy(elements, 0, e, 0, elements.length);
            elements = e;
            System.out.println("Expanded to " + elements.length);
        }
    }

    public Object get(int index) {
        if (index >= 0 && index < elements.length) {
            return elements[index];
        } else {
            throw new ArrayIndexOutOfBoundsException("Wrong input index.");
        }
    }

    public int size() {
        return elements.length;
    }

    public boolean isEmpty() {
        for (Object element : elements) {
            if (element == null) {
                return false;
            }
        }
        return true;
    }

    //unsafe to use at all
    public void compress() {
        Object[] newArray = new Object[elements.length / 2];
        System.arraycopy(elements, 0, newArray, 0, newArray.length);
        elements = newArray;
        System.out.println("Compressed to " + newArray.length);
    }
}
