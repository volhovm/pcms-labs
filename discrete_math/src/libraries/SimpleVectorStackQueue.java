package libraries;

/**
 * Created by volhovm on 3/19/14.
 */
public class SimpleVectorStackQueue {
    SimpleVectorStack stackOne = new SimpleVectorStack();
    SimpleVectorStack stackTwo = new SimpleVectorStack();

    public void put(Object o) {
        stackOne.push(o);
    }

    public Object get() {
        if (stackTwo.isEmpty()) {
            replaceAllElements();
        }
        return stackTwo.pop();
    }

    private void replaceAllElements() {
        while (!stackOne.isEmpty()) {
            stackTwo.push(stackOne.pop());
        }
    }

    public boolean isEmpty() {
        return (stackOne.isEmpty() && stackTwo.isEmpty());
    }

    public int size() {
        return stackOne.size() + stackTwo.size();
    }
}
