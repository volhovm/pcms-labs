package labs.segmenttree;

import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * @author volhovm
 *         Created on 30.04.14
 */
public class A_RMQ {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("rmq.in"));
        scout = new PrintWriter(new File("rmq.out"));
        int n = scin.nextInt();
        SegmentTree segmentTree =
                new SegmentTree(n, Integer.MAX_VALUE);
        for (int i = 0; i < n; i++) {
            segmentTree.set(i, scin.nextInt());
        }
        try {
            while (true) {
                String s = scin.next();
                switch (s){
                    case "set":
                        segmentTree.set(scin.nextInt() - 1, scin.nextInt());
                        break;
                    case "min":
                        scout.println(segmentTree.get(scin.nextInt() - 1, scin.nextInt()));
                        break;
                }
            }
        } catch (IOException ignored){
            System.err.println("EOF");
        }
        scout.close();
    }

    private static class SegmentTree {
        private int[] mainArray;
        private final int neutral;
        private int capacity;

        public SegmentTree(int size, int neutralElement) {
            //noinspection StatementWithEmptyBody
            for (capacity = 1; capacity < size; capacity *= 2) {
            }
            //noinspection unchecked
            mainArray = new int[2 * capacity];
            Arrays.fill(mainArray, neutralElement);
            neutral = neutralElement;
        }

        public void set(int index, int value) {
            mainArray[capacity + index] = value;
            int v = capacity + index;
            v = v / 2;
            while (v > 0) {
                mainArray[v] = Math.min(mainArray[2 * v], mainArray[2 * v + 1]);
                v = v / 2;
            }
        }

        private int get(int currL, int currR, int l, int r, int node){
            if (l <= currL && currR <= r) {
                return mainArray[node];
            }
            if (r <= currL || l >= currR){
                return neutral;
            }
            int mid = (currL + currR) / 2;
            return Math.min(get(currL, mid, l, r, node * 2), get(mid, currR, l, r, node * 2 + 1));
        }

        public int get(int left, int right){
            return get(0, capacity, left, right, 1);
        }
    }

    public static FastScanner scin;
    public static PrintWriter scout;

    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) throws IOException {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() throws IOException {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                } catch (NullPointerException e) {
                    throw new IOException("EOF", e);
                }

            }
            return st.nextToken();
        }

        int nextInt() throws IOException {
            return Integer.parseInt(next());
        }

        double nextDouble() throws IOException {
            return Double.parseDouble(next());
        }
    }
}
