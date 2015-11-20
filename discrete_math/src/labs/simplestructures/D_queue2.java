package labs.simplestructures; /**
 *   Created by volhovm on 3/8/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class D_queue2 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("queue2.in"));
        scout = new PrintWriter(new File("queue2.out"));
        SimpleLinkedListQueue queue = new SimpleLinkedListQueue();
        int N = scin.nextInt();
        for (int i = 0; i < N; i++) {
            String currentString = scin.next();
            if (currentString.equals("+")) {
                queue.put(scin.next());
            } else if (currentString.equals("-")) {
                scout.println(String.valueOf(queue.get()));
            }
        }
        scout.close();
    }

    static class SimpleLinkedListQueue {
        private class Node {
            Node(Object object, Node nextObject) {
                this.object = object;
                this.nextObject = nextObject;
            }

            private Object object;

            public Object getObject() {
                return object;
            }

            public void setNextObject(Node nextObject) {
                this.nextObject = nextObject;
            }

            private Node nextObject;

            public Node getNextObject() {
                return nextObject;
            }
        }

        private Node tail = null;
        private Node head = tail;
        private int size = 0;

        public void put(Object o) {
            if (this.isEmpty()) {
                tail = new Node(o, null);
                head = tail;
            } else {
                Node current = tail;
                tail = new Node(o, null);
                current.setNextObject(tail);
            }
            size++;
        }

        public Object get() {
            if (!isEmpty()) {
                if (size == 1) {
                    Object ret = head.getObject();
                    head = tail = null;
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
                return head.getObject();
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
