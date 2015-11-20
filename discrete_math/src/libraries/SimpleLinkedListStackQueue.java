package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleLinkedListStackQueue {
    SimpleLinkedListStack stackOne = new SimpleLinkedListStack();
    SimpleLinkedListStack stackTwo = new SimpleLinkedListStack();

    public void put(Object o) {
        stackOne.push(o);
    }

    public Object get() {
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
