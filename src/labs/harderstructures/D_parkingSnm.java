package labs.harderstructures;
/**
 *   Created by volhovm on 27.03.14
 */

import java.io.*;
import java.util.StringTokenizer;

public class D_parkingSnm {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("parking.in"));
        scout = new PrintWriter(new File("parking.out"));
        int n = scin.nextInt();
        DSU dsu = new DSU(n);
        for (int i = 0; i < n; i++) {
            int freeSpace = dsu.get(scin.nextInt() - 1);
            dsu.union(freeSpace, (freeSpace + 1) % n);
            scout.print(freeSpace + 1 + " ");
        }
        scout.close();
    }

    static public class DSU {
        DSUNode[] nodes;

        public DSU(int n) {
            nodes = new DSUNode[n];
            for (int i = 0; i < n; i++) {
                nodes[i] = new DSUNode(i, i, 1, -1);
            }
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
            if (aFatherIndex == bFatherIndex) {
                return;
            }
            DSUNode aFather = nodes[aFatherIndex];
            DSUNode bFather = nodes[bFatherIndex];
            aFather.parentIndex = bFatherIndex;
            bFather.min = StrictMath.min(aFather.min, bFather.min);
            bFather.max = StrictMath.max(aFather.max, bFather.max);
            bFather.count += aFather.count;
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
