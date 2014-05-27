package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedListStackMinQueueMin {
    SimpleLinkedListStackMin stackOne = new SimpleLinkedListStackMin();
    SimpleLinkedListStackMin stackTwo = new SimpleLinkedListStackMin();

    public void put(long l) {
        stackOne.push(l);
    }

    public long getMin() {
        if (stackOne.isEmpty() || stackTwo.isEmpty()) {
            return stackOne.isEmpty() ? stackTwo.getMin() : stackOne.getMin();
        } else {
            return Math.min(stackOne.getMin(), stackTwo.getMin());
        }
    }

    public long get() {
        if (stackTwo.isEmpty()) {
            replaceAllElements();
        }
        return stackTwo.pop();
    }

    public Object peek() {
        if (stackTwo.isEmpty()) {
            replaceAllElements();
        }
        return stackTwo.peek();
    }

    private void replaceAllElements() {
        while (!stackOne.isEmpty()) {
            stackTwo.push(stackOne.pop());
        }
    }

    public boolean isEmpty() {
        return stackOne.isEmpty() && stackTwo.isEmpty();
    }

    public int size() {
        return stackOne.size() + stackTwo.size();
    }
}
