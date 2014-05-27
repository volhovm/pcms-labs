package labs.simplestructures;

import java.io.*;
import java.util.StringTokenizer;

/**
 * Created by volhovm on 3/7/14.
 */
public class B_stack2 {
    public static void main(String[] args) throws IOException {
        FastScanner scin = new FastScanner(new File("stack2.in"));
        FileWriter scout = new FileWriter("stack2.out");
        SimpleLinkedListStack stack = new SimpleLinkedListStack();
        int N = scin.nextInt();
        for (int i = 0; i < N; i++) {
            String currentString = scin.next();
            if (currentString.equals("+")) {
                stack.push(scin.next());
            } else if (currentString.equals("-")) {
                scout.write(stack.pop() + "\r\n");
            }
        }
        scout.close();
    }

    static class SimpleLinkedList {
        private class Node {
            Node(Object object, Node prevObject) {
                this.object = object;
                this.prevObject = prevObject;
            }

            private Object object;

            public Object getObject() {
                return object;
            }

            private Node prevObject;

            public Node getLinkObject() {
                return prevObject;
            }
        }

        private Node headNode = null;
        private Node tailNode = headNode;
        private int capacity = 0;

        public void add(Object o) {
            if (capacity == 0) {
                headNode = new Node(o, null);
                tailNode = headNode;
                capacity++;
            } else {
                tailNode = new Node(o, tailNode);
                capacity++;
            }
        }

        public Object getLast() {
            if (capacity > 1) {
                Object ret = tailNode.getObject();
                Object redundant = tailNode;
                tailNode = tailNode.getLinkObject();
                redundant = null;
                capacity--;
                return ret;
            } else if (capacity == 1) {
                Object ret = tailNode.getObject();
                headNode = null;
                tailNode = headNode;
                capacity--;
                return ret;
            } else {
                throw new ArrayIndexOutOfBoundsException("Can't find Node, 0 elements in List");
            }
        }

        public Object peek() {
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
    }

    static class SimpleLinkedListStack {
        private SimpleLinkedList list = new SimpleLinkedList();

        public void push(Object o) {
            list.add(o);
        }

        public Object pop() {
            return list.getLast();
        }

        public boolean isEmpty() {
            return list.isEmpty();
        }

        public int size() {
            return list.size();
        }
    }


    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }
    }
}
