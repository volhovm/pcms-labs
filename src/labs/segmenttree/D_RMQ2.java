package labs.segmenttree;
/**
 * @author volhovm
 * Created on 05.05.14
 */

import java.io.*;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.StringTokenizer;


//Not working, too bored to write useless stuff
public class D_RMQ2 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("rmq2.in"));
        scout = new PrintWriter(new File("rmq2.out"));
        int n = scin.nextInt();
        SegmentTree segmentTree =
                new SegmentTree(n, Integer.MAX_VALUE);
        segmentTree.rangeSet(n);
        try {
            while (true) {
                String s = scin.next();
                switch (s){
                    case "set":
                        segmentTree.set(scin.nextInt() - 1, scin.nextInt(), scin.nextInt());
                        break;
                    case "add":
                        segmentTree.add(scin.nextInt() - 1, scin.nextInt(), scin.nextInt());
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
        private class Node{
            int a, setting;

            private Node(int a, int setting) {
                this.a = a;
                this.setting = setting;
            }

            private Node(int a) {
                this.a = a;
            }

            public Node min(Node node) {
                if (a < node.a){
                    return this;
                } else return node;
            }
        }
        private Node[] mainArray;
        private final int neutral;
        private int capacity;

        public SegmentTree(int size, int neutralElement) {
            //noinspection StatementWithEmptyBody
            for (capacity = 1; capacity < size; capacity *= 2) {
            }
            //noinspection unchecked
            mainArray = new Node[2 * capacity];
            Arrays.fill(mainArray, new Node(neutralElement));
            neutral = neutralElement;
        }

        private void push(int node){
            if (mainArray[node].setting != 0){
                mainArray[node * 2].setting = mainArray[node * 2 + 1].setting = mainArray[node].setting;
                mainArray[node].setting = 0;
            }
        }

        public void set(int left, int right, int value){
            set(0, capacity, left, right, 1, value);
        }

        public void set(int currL, int currR, int l, int r, int node, int value) {
            if (l >= r) return;
            if (l == currL && r == currR){
                mainArray[node].a = value;
            } else {
                push(node);
                int mid = (currL + currR) / 2;
                set(currL, mid, l, Math.min(r, mid), node * 2, value);
                set(mid, currR, Math.max(l, mid), r, node * 2 + 1, value);
            }
        }

        public void add(int l, int r, int addValue){
            add(0, capacity, l, r, 1, addValue);
        }

        public void add(int currL, int currR, int l, int r, int node, int addValue) {
            if (l >=r) return;
            if (l == currL && r == currR){
                mainArray[node].a += addValue;
            }
            else {
                int mid = (currL + currR) / 2;
                add(currL, mid, l, Math.min(r, mid), node * 2, addValue);
                add(mid, currR, Math.max(l, mid), r, node * 2 + 1, addValue);
            }
        }

        public void rangeSet(int n) throws IOException {
            for (int i = 0; i < n; i++) {
                mainArray[capacity+i] = new Node(scin.nextInt());
            }
            int i = capacity/2;
            while (i != 0){
                for (int j = i; j < i * 2; j++) {
                    mainArray[j] = mainArray[j*2].min(mainArray[j * 2 + 1]);
                }
                i = i / 2;
            }
        }

        private int get(int currL, int currR, int l, int r, int node){
            if (l <= currL && currR <= r) {
                return mainArray[node].a + mainArray[node].setting;
            }
            if (r <= currL || l >= currR){
                return neutral;
            }
            push(node);
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
                br = new BufferedReader(new InputStreamReader(new FileInputStream(f), Charset.forName("UTF-8")));
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
