package labs.segmenttree;
/**
 *   Created by volhovm on 3/23/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class F_parking {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("parking.in"));
        scout = new PrintWriter(new File("parking.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        RangedTree parking = new RangedTree(n);
        for (int i = 0; i < m; i++) {
            String s = scin.next();
            switch (s){
                case "enter":
                    scout.println(parking.circularSetTrue(scin.nextInt() - 1) + 1);
                    break;
                case "exit":
                    parking.set(scin.nextInt() - 1, false);
            }
        }
        scout.close();
    }

    static class RangedTree {
        boolean[] mainArray; //true == there is a car somewhere
        int capacity;

        public RangedTree(int a) {
            for (capacity = 1; capacity < a; capacity *= 2) {
            }
            mainArray = new boolean[2 * capacity];
            for (int i = a; i < capacity; i++) {
                set(i,  true);
            }
        }

        public long circularSetTrue(int index) {
            int v = index;
            if (!mainArray[v + capacity]) {
                set(v, true);
            } else {
                boolean lastRouteFrom = false; //true = left, false = right
                v = v + capacity;
                if (v % 2 == 0) {
                    lastRouteFrom = true;
                }
                v = v / 2;
                while (v != 1) {
                    if (lastRouteFrom && !mainArray[v * 2 + 1]) {
                        v = v * 2 + 1;
                        break;
                    }
                    lastRouteFrom = v % 2 == 0;
                    v = v / 2;
                }
                if (v == 1) {
                    if (lastRouteFrom && !mainArray[3]) {
                        v = 3;
                    } else {
                        if (mainArray[2]) {
                            v = 3;
                        } else {
                            v = 2;
                        }
                    }
                }
                while (v < capacity) {
                    if (!mainArray[v * 2]) {
                        v = v * 2;
                    } else {
                        v = v * 2 + 1;
                    }
                }
                v = v - capacity;
                set(v, true);
            }
            return v;
        }

        public void set(int index, boolean value) {
            mainArray[capacity + index] = value;
            int v = capacity + index;
            v = v / 2;
            while (v > 0) {
                mainArray[v] = mainArray[2 * v] && mainArray[2 * v + 1];
                v = v / 2;
            }
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
