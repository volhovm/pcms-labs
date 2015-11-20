package labs.simplestructures;

import java.io.*;
import java.util.StringTokenizer;

public class F_postfix {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("postfix.in"));
        scout = new PrintWriter(new File("postfix.out"));
        SimpleLinkedListStack<Long> stack = new SimpleLinkedListStack<Long>();
        try {
            while (true) {
                String current = scin.next();
                if (current.equals("+")) {
                    stack.push(stack.pop() + stack.pop());
                } else if (current.equals("-")) {
                    stack.push(-stack.pop() + stack.pop());
                } else if (current.equals("*")) {
                    stack.push(stack.pop() * stack.pop());
                } else {
                    stack.push(Long.parseLong(current));
                }
            }
        } catch (NullPointerException exc) {
            System.out.println("--- EOF ---");
        }
        scout.println(String.valueOf(stack.pop()));
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

        public Object peekLast() {
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

    static class SimpleLinkedListStack<MyType> {
        private SimpleLinkedList list = new SimpleLinkedList();

        public void push(MyType o) {
            list.add(o);
        }

        public MyType pop() {
            return (MyType) list.getLast();
        }

        public MyType peek() {
            return (MyType) list.peekLast();
        }

        public boolean isEmpty() {
            return list.isEmpty();
        }

        public int size() {
            return list.size();
        }
    }


    public static FastScanner scin;
    public static PrintWriter scout;

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

        double nextDouble() {
            return Double.parseDouble(next());
        }
    }
}
