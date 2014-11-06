package labs.spantrees;/**
 * @author volhovm
 * Created on 11/5/14
 */


import java.io.*;
import java.util.*;

public class B_spanTree2 {
    private static class Pair { int _1, _2;  private Pair(int a, int b) { this._1 = a; this._2 = b; } }  private static class Pair2 { Pair _1; int _2;  private Pair2(Pair a, int b) { this._1 = a; this._2 = b;
        }
    }

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("spantree2.in"));
        scout = new PrintWriter(new File("spantree2.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        ArrayList<Pair2> g = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            g.add(new Pair2(new Pair(scin.nextInt() - 1, scin.nextInt() - 1), scin.nextInt()));
        }
        long cost = 0;
        ArrayList<Pair> res = new ArrayList<>();
        Collections.sort(g, new Comparator<Pair2>() {
            @Override
            public int compare(Pair2 pair, Pair2 pair2) {
                return Integer.compare(pair._2, pair2._2);
            }
        });
        DSU dsu = new DSU(n);
        for (int i = 0; i < m; i++) {
            int a = g.get(i)._1._1;
            int b = g.get(i)._1._2;
            int w = g.get(i)._2;
            if (dsu.get(a) != dsu.get(b)) {
                cost = cost + w;
                res.add(g.get(i)._1);
                dsu.union(a, b);
            }
        }
        scout.print(cost);
        scout.close();
    }

    private static class DSU {
        DSUNode[] nodes;

        public DSU(int n) {
            nodes = new DSUNode[n];
            for (int i = 0; i < n; i++) {
                nodes[i] = new DSUNode(i, i, 1, -1);
            }
        }

        public void printAllNodeData(int index, PrintWriter printWriter) {
            DSUNode classRepresentative = nodes[get(index)];
            printWriter.println(classRepresentative.min + " " + classRepresentative.max + " " + classRepresentative.count);
        }

        public int get(int index) {
            if (nodes[index].parentIndex == -1) {
                return index;
            } else {
                int current = get(nodes[index].parentIndex);
                nodes[index].parentIndex = current;
                return current;
            }
        }

        public void union(int a, int b) {
            int aFatherIndex = get(a);
            int bFatherIndex = get(b);
            if (aFatherIndex == bFatherIndex) return;
            DSUNode aFather = nodes[aFatherIndex];
            DSUNode bFather = nodes[bFatherIndex];
            if (aFather.count > bFather.count) {
                bFather.parentIndex = aFatherIndex;
                aFather.min = StrictMath.min(aFather.min, bFather.min);
                aFather.max = StrictMath.max(aFather.max, bFather.max);
                aFather.count += bFather.count;
            } else {
                aFather.parentIndex = bFatherIndex;
                bFather.min = StrictMath.min(aFather.min, bFather.min);
                bFather.max = StrictMath.max(aFather.max, bFather.max);
                bFather.count += aFather.count;
            }
        }

        private class DSUNode {
            int min, max, count, parentIndex;

            private DSUNode(int min, int max, int count, int parentIndex) {
                this.min = min;
                this.max = max;
                this.count = count;
                this.parentIndex = parentIndex;
            }
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
