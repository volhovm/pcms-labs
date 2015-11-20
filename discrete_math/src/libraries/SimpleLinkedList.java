package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedList<T> {
    private Node headNode = null;
    private Node tailNode = headNode;
    private int capacity = 0;

    public void add(T o) {
        if (capacity == 0) {
            headNode = new Node(o, null);
            tailNode = headNode;
            capacity++;
        } else {
            tailNode = new Node(o, tailNode);
            capacity++;
        }
    }

    public T getLast() {
        if (capacity > 1) {
            T ret = tailNode.getObject();
            Node redundant = tailNode;
            tailNode = tailNode.getLinkObject();
            redundant = null;
            capacity--;
            return ret;
        } else if (capacity == 1) {
            T ret = tailNode.getObject();
            headNode = null;
            tailNode = headNode;
            capacity--;
            return ret;
        } else {
            throw new ArrayIndexOutOfBoundsException("Can't find Node, 0 elements in List");
        }
    }

    public T peekLast() {
        if (capacity > 0) {
            return tailNode.getObject();
        } else {
            throw new ArrayIndexOutOfBoundsException("Can't find Node, 0 elements in List");
        }
    }

    public boolean isEmpty() {
        return capacity == 0;
    }

    public int size() {
        return capacity;
    }

    private class Node {
        private T object;
        private Node prevObject;

        Node(T object, Node prevObject) {
            this.object = object;
            this.prevObject = prevObject;
        }

        public T getObject() {
            return object;
        }

        public Node getLinkObject() {
            return prevObject;
        }
    }
}
