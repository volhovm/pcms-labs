package labs.spantrees;

import java.io.*;
import java.util.*;

/**
 * @author volhovm
 *         Created on 11/5/14
 */

public class A_spanTree {
    static class Pair {
        int a;
        double b;

        private Pair(int a, double b) {
            this.a = a;
            this.b = b;
        }

    }

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("spantree.in"));
        scout = new PrintWriter(new File("spantree.out"));
        int n = scin.nextInt();
        int[][] graph = new int[n][2];
        for (int i = 0; i < n; i++) {
            graph[i][0] = scin.nextInt();
            graph[i][1] = scin.nextInt();
        }
        boolean[] used = new boolean[n];
        double[] min_e = new double[n];
        Arrays.fill(min_e, Double.MAX_VALUE);
        int[] sel_e = new int[n];
        Arrays.fill(sel_e, -1);
        min_e[0] = 0;
        for (int i = 0; i < n; i++) {
            int v = -1;
            for (int j = 0; j < n; j++) {
                if (!used[j] && (v == -1 || min_e[j] < min_e[v])) v = j;
            }
            used[v] = true;
            for (int to = 0; to < n; to++) {
                if (used[to]) continue;
                double w = to == v ? Double.MAX_VALUE : Math.sqrt((graph[v][1] - graph[to][1]) * (graph[v][1] - graph[to][1])
                        + (graph[v][0] - graph[to][0]) * (graph[v][0] - graph[to][0]));
                if (w < min_e[to]) {
                    min_e[to] = w;
                    sel_e[to] = v;
                }
            }
        }
        double wsum = 0;
        for (int i = 0; i < n; i++) {
            wsum += min_e[i];
        }
        scout.print(wsum);
        scout.close();
    }

    private static FastScanner scin;
    private static PrintWriter scout;

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
