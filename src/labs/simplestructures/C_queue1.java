package labs.simplestructures; /**
 *   Created by volhovm on 3/8/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class C_queue1 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("queue1.in"));
        scout = new PrintWriter(new File("queue1.out"));
        ArrayDeque stackQueue = new ArrayDeque();
        int N = scin.nextInt();
        for (int i = 0; i < N; i++) {
            String currentString = scin.next();
            if (currentString.equals("+")) {
                stackQueue.addLast(scin.next());
            } else if (currentString.equals("-")) {
                scout.println(stackQueue.removeFirst());
            }
        }
        scout.close();
    }


    static class ArrayDeque {
        private int n = 0;
        private Object[] mainArray = (Object[]) new Object[10];
        private int head = 1;
        private int tail = 0;

        public void addLast(Object o) {
            if (n + 1 > mainArray.length) {
                expandArray();
            }
            if (head == 0) {
                head = mainArray.length - 1;
            } else {
                head--;
            }
            mainArray[head] = o;
            n++;
        }

        public void addFirst(Object o) {
            if (n + 1 > mainArray.length) {
                expandArray();
            }
            tail = (tail + 1) % (mainArray.length);
            mainArray[tail] = o;
            n++;
        }

        private void expandArray() {
            Object[] tempArray = new Object[n * 10];
            if (head == 0 && tail == n) {
                System.arraycopy(mainArray, 0, tempArray, 0, n);
            } else {
                System.arraycopy(mainArray, head, tempArray, 0, mainArray.length - head);
                System.arraycopy(mainArray, 0, tempArray, mainArray.length - head, tail + 1);
            }
            head = 0;
            tail = n - 1;
            mainArray = tempArray;
        }

        public Object removeLast() {
            assert n > 0;
            Object ret = mainArray[head];
            head = (head + 1) % mainArray.length;
            n--;
            return ret;
        }

        public Object removeFirst() {
            assert n > 0;
            Object ret = mainArray[tail];
            if (tail == 0) {
                tail = mainArray.length - 1;
            } else {
                tail--;
            }
            n--;
            return ret;
        }

        public Object peekLast() {
            assert n > 0;
            return mainArray[head];
        }

        public Object peekFirst() {
            assert n > 0;
            return mainArray[tail];
        }

        public boolean isEmpty() {
            return n == 0;
        }

        public int size() {
            return n;
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
