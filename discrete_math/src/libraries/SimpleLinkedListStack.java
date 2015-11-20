package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedListStack {
    private SimpleLinkedList list = new SimpleLinkedList();

    public void push(Object o) {
        list.add(o);
    }

    public Object pop() {
        return list.getLast();
    }

    public Object peek() {
        return list.peekLast();
    }

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public int size() {
        return list.size();
    }
}
