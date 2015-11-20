package labs.flows;
/**
 * @author volhovm
 * Created on 11/23/14
 */

import java.io.*;
import java.util.*;

public class A_matching {
    static private boolean[] used;
    static private ArrayList<ArrayList<Integer>> g;
    static private int[] ans;

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("matching.in"));
        scout = new PrintWriter(new File("matching.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        int k = scin.nextInt();
        g = new ArrayList<>(n);
        for (int i = 0; i < n; i++) g.add(new ArrayList<Integer>());
        for (int i = 0; i < k; i++) {
            int a = scin.nextInt() - 1;
            int b = scin.nextInt() - 1;
            g.get(a).add(b);
        }
        ans = new int[m];
        Arrays.fill(ans, -1);
        used = new boolean[n];
        for (int i = 0; i < n; i++) {
            Arrays.fill(used, false);
            findMatching(i);
        }
        int count = 0;
        for (int i = 0; i < m; i++) {
            if (ans[i] != -1) count++;
        }
        scout.write(String.valueOf(count));
        scout.close();
    }

    private static boolean findMatching(int v) {
        if (used[v]) return false;
        used[v] = true;
        for (int i = 0; i < g.get(v).size(); i++) {
            int to = g.get(v).get(i);
            if (ans[to] == -1 || findMatching(ans[to])) {
                ans[to] = v;
                return true;
            }
        }
        return false;
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
