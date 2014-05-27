package labs.simplestructures; /**
 *   Created by volhovm on 3/8/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class G_queuemin2 {
    public static FastScanner scin;
    public static PrintWriter scout;

    public static void main(String[] args) throws IOException {
        double delta = -1;
        for (int j = 0; j < 100; j++) {
            long startTime = System.currentTimeMillis();
            new G_queuemin2().execute();
            //            Runtime.getRuntime().gc();
            delta = delta == -1 ? System.currentTimeMillis() - startTime : (delta + System.currentTimeMillis() - startTime) / 2;
            System.out.println("Delta = " + String.valueOf(System.currentTimeMillis() - startTime));
        }
        System.out.println("\r\nAverage delta = " + String.valueOf(delta));
    }

    private void execute() throws FileNotFoundException {
        scin = new FastScanner(new File("queuemin2.in"));
        scout = new PrintWriter(new File("queuemin2.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        int k = scin.nextInt();
        int a = scin.nextInt();
        int b = scin.nextInt();
        int c = scin.nextInt();
        SimpleDoubleLinkedListMin queueMin = new SimpleDoubleLinkedListMin();
        long sum = 0;
        int current = 0, prev = 0, preprev = 0;
        for (int i = 0; i < n; i++) {
            if (i < k) {
                current = scin.nextInt();
            } else {
                current = a * preprev + b * prev + c;
            }
            queueMin.addFirst(current);
            if (i > m - 2) {
                sum += queueMin.getMin();
                queueMin.silentRemoveFirst();
            }
            preprev = prev;
            prev = current;
        }
        scout.println(String.valueOf(sum));
        scout.close();
    }

    class SimpleDoubleLinkedListMin {
        private Node headNode = null;
        private Node tailNode = headNode;
        private int capacity = 0;

        public void silentRemoveFirst() {
            headNode.number--;
            capacity--;
            if (capacity == 0) {
                tailNode = headNode = null;
            } else if (headNode.number == 0) {
                headNode.nextObject.prevObject = headNode.prevObject;
                headNode = headNode.nextObject;
            }
        }

        public int size() {
            return capacity;
        }

        public void addFirst(int o) {
            Node current = new Node(o);
            if (this.isEmpty()) {
                current.number = 1;
                tailNode = headNode = current;
            } else {
                int counter = 1;
                Node tempTail = tailNode;
                while (tempTail != null && tempTail.minimum >= o) {
                    counter += tempTail.number;
                    tempTail = tempTail.prevObject;
                }
                current.number = counter;
                if (counter > capacity) {
                    tailNode = headNode = current;
                } else if (counter == capacity) {
                    headNode.nextObject = current;
                    current.prevObject = headNode;
                    tailNode = current;
                } else { //ordinary add to the end of queue
                    tempTail.nextObject = current;
                    current.prevObject = tempTail;
                    tailNode = current;
                }
            }
            capacity++;
        }

        public boolean isEmpty() {
            return capacity == 0;
        }

        public int getMin() {
            return headNode.minimum;
        }

        class Node {
            private int minimum, number;
            private Node prevObject, nextObject;

            Node(int minimum) {
                this.minimum = minimum;
            }
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

        int nextInt() {
            return Integer.parseInt(next());
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

        double nextDouble() {
            return Double.parseDouble(next());
        }

        public long nextLong() {
            return Long.parseLong(next());
        }
    }
}
