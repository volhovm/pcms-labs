package labs.simplestructures; /**
 *   Created by volhovm on 3/9/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class A_stack1 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("stack1.in"));
        scout = new PrintWriter(new File("stack1.out"));
        SimpleVectorStack stack = new SimpleVectorStack();
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


    static class SimpleVector {
        private Object[] elements = new Object[10];

        public void set(Object o, int index) {
            ensureCapacity(index);
            elements[index] = o;
        }

        private void ensureCapacity(int capacity) {
            if (elements.length <= capacity) {
                Object[] e = new Object[capacity * 2];
                System.arraycopy(elements, 0, e, 0, elements.length);
                elements = e;
                System.out.println("Expanded to " + elements.length);
            }
        }

        public Object get(int index) {
            if (index >= 0 && index < elements.length) {
                return elements[index];
            } else {
                throw new ArrayIndexOutOfBoundsException("Wrong input index.");
            }
        }

        public int size() {
            return elements.length;
        }

        public boolean isEmpty() {
            for (Object element : elements) {
                if (element == null) {
                    return false;
                }
            }
            return true;
        }

        public void compress() {
            Object[] newArray = new Object[elements.length / 2];
            System.arraycopy(elements, 0, newArray, 0, newArray.length);
            elements = newArray;
            System.out.println("Compressed to " + newArray.length);
        }
    }


    static class SimpleVectorStack {
        SimpleVector vector = new SimpleVector();
        int size = 0;

        public void push(Object o) {
            vector.set(o, size++);
        }

        public Object pop() {
            if (!this.isEmpty()) {
                ensureCanCompress();
                Object ret = vector.get(--size);
                vector.set(null, size + 1);
                return ret;
            } else {
                throw new ArrayIndexOutOfBoundsException();
            }
        }


        private void ensureCanCompress() {
            if (size * 4 <= vector.size()) {
                vector.compress();
            }
        }

        public int size() {
            return this.size;
        }

        public boolean isEmpty() {
            return size == 0;
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
