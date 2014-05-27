package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedListStackMin {
    private SimpleLinkedList<ObjectPair> list = new SimpleLinkedList<ObjectPair>();

    public void push(long value) {
        list.add(list.isEmpty() ? new ObjectPair(value, value) : new ObjectPair(Math.min(value,
                                                                                         list.peekLast()
                                                                                             .getMinToJ()),
                                                                                value
        ));
    }

    public long pop() {
        if (!list.isEmpty()) {
            return list.getLast().getObject();
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public long getMin() {
        if (!list.isEmpty()) {
            return list.peekLast().getMinToJ();
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public long peek() {
        if (!list.isEmpty()) {
            return list.peekLast().getObject();
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public int size() {
        return list.size();
    }

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public class ObjectPair {
        private long minToJ;
        private long object;

        public ObjectPair(long minToJ, long object) {
            this.minToJ = minToJ;
            this.object = object;
        }

        public long getMinToJ() {
            return minToJ;
        }

        public void setMinToJ(int minToJ) {
            this.minToJ = minToJ;
        }

        public long getObject() {
            return object;
        }
    }
}
