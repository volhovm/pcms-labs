package labs.simplestructures; /**
 *   Created by volhovm on 3/8/14
 */

import java.io.*;
import java.util.HashMap;
import java.util.StringTokenizer;

public class H_quack {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("quack.in"));
        scout = new PrintWriter(new File("quack.out"));
        MyLinkedList<Long> mainQueue = new MyLinkedList<Long>();
        HashMap<Character, Long> registers = new HashMap<Character, Long>();
        HashMap<String, Integer> jumpLabels = new HashMap<String, Integer>();
        String[] programCode = new String[100500];
        // Jumps selection
        int programSize = 0;
        try {
            while (true) {
                String current = scin.next();
                programCode[programSize++] = current;
                if (current.startsWith(":")) {
                    jumpLabels.put(current.substring(1), programSize - 1);
                }
            }
        } catch (NullPointerException exc) {
            System.out.println("EOF");
        }
        for (int i = 0; i < programSize; i++) {
            String currentString = programCode[i];
            if (currentString.equals("+")) {
                mainQueue.put((mainQueue.get() + mainQueue.get()) % 65536);
            } else if (currentString.equals("-")) {
                long a = mainQueue.get();
                long b = mainQueue.get();
                long c = (a - b) % 65536;
                if (c < 0) {
                    c = 65536 + c;
                }
                mainQueue.put(c);
            } else if (currentString.equals("*")) {
                mainQueue.put((mainQueue.get() * mainQueue.get()) % 65536);
            } else if (currentString.equals("/")) { //throws exception t8
                long a = mainQueue.get();
                long b = mainQueue.get();
                if (a == 0 && b == 0) {
                    mainQueue.put(0l);
                } else {
                    mainQueue.put((a / b) % 65536);
                }
            } else if (currentString.equals("%")) //throws exception 8t
            {
                long a = mainQueue.get();
                long b = mainQueue.get();
                if (a == 0 && b == 0) {
                    mainQueue.put(0l);
                } else {
                    mainQueue.put((a % b) % 65536);
                }
            } else if (currentString.startsWith(">")) {
                registers.put(currentString.charAt(1), mainQueue.get());
            } else if (currentString.startsWith("<")) {
                mainQueue.put(registers.get(currentString.charAt(1)));
            } else if (currentString.equals("P")) {
                scout.println(String.valueOf(mainQueue.get())); //throws too
            } else if (currentString.startsWith("P")) {
                scout.println(registers.get(currentString.charAt(1)));
            } else if (currentString.equals("C")) {
                scout.print((char) (mainQueue.get() % 256)); //not sure if gonna work
            } else if (currentString.startsWith("C")) {
                scout.print((char) (registers.get(currentString.charAt(1)) % 256)); //and this
            } else if (currentString.startsWith(":")) {
            } else if (currentString.startsWith("J")) {
                i = jumpLabels.get(currentString.substring(1));
            } else if (currentString.startsWith("Z")) {
                if (registers.get(currentString.charAt(1)) == 0) {
                    i = jumpLabels.get(currentString.substring(2));
                }
            } else if (currentString.startsWith("E")) {
                if (registers.get(currentString.charAt(1)).equals(registers.get(currentString.charAt(
                    2)))) {
                    i = jumpLabels.get(currentString.substring(3));
                    continue;
                }
            } else if (currentString.startsWith("G")) {
                if (registers.get(currentString.charAt(1)) > registers.get(currentString.charAt(2))) {
                    i = jumpLabels.get(currentString.substring(3));
                    continue;
                }
            } else if (currentString.equals("Q")) {
                break;
            } else {
                mainQueue.put(Long.parseLong(currentString) % 65536);
            }
        }
        scout.close();
    }


    static class MyLinkedList<Type> extends ArrayDeque<Type> {
        public void put(Type a) {
            addFirst(a);
        }

        public Type get() {
            return removeLast();
        }
    }

    public static class ArrayDeque<T> {
        private int n = 0;
        private T[] mainArray = (T[]) new Object[10];
        private int head = 1;
        private int tail = 0;

        public void addLast(T o) {
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

        public void addFirst(T o) {
            if (n + 1 > mainArray.length) {
                expandArray();
            }
            tail = (tail + 1) % (mainArray.length);
            mainArray[tail] = o;
            n++;
        }

        private void expandArray() {
            T[] tempArray = (T[]) new Object[n * 10];
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

        public T removeLast() {
            assert n > 0;
            T ret = mainArray[head];
            head = (head + 1) % mainArray.length;
            n--;
            return ret;
        }

        public T removeFirst() {
            assert n > 0;
            T ret = mainArray[tail];
            if (tail == 0) {
                tail = mainArray.length - 1;
            } else {
                tail--;
            }
            n--;
            return ret;
        }

        public T peekLast() {
            assert n > 0;
            return mainArray[head];
        }

        public T peekFirst() {
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
    }

    private static class RegisterContainer {
        private int[] registers = new int[26];

        public void put(char register, long value) {
            registers[((int) register) - 'a'] = (int) value;
        }

        public long get(char register) {
            return registers[((int) register) - 'a'];
        }
    }
}
