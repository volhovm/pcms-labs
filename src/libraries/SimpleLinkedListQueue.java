package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedListQueue {
    private Node tail = null;
    private Node preTail = tail;
    private Node head = preTail;
    private int size = 0;

    public void put(Object o) {
        tail = new Node(o, null);
        if (this.isEmpty()) {
            preTail = tail;
            head = preTail;
        } else if (size == 1) {
            preTail.setNextObject(tail);
            head = preTail;
        } else {
            preTail = preTail.getNextObject();
            preTail.setNextObject(tail);
        }
        size++;
    }

    public Object get() {
        if (!isEmpty()) {
            if (size == 1) {
                Object ret = head.getObject();
                head = preTail = tail = null;
                size--;
                return ret;
            } else {
                Node ret = head;
                head = ret.getNextObject();
                ret.setNextObject(null);
                size--;
                return ret.getObject();
            }

        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public Object peek() {
        if (!isEmpty()) {
            return tail.getObject();
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    private class Node {
        private Object object;
        private Node nextObject;

        Node(Object object, Node nextObject) {
            this.object = object;
            this.nextObject = nextObject;
        }

        public Object getObject() {
            return object;
        }

        public Node getNextObject() {
            return nextObject;
        }

        public void setNextObject(Node nextObject) {
            this.nextObject = nextObject;
        }
    }
}
