package labs.matroids;

import javafx.util.Pair;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.StringTokenizer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * @author volhovm
 *         Created on 5/4/15
 */
public class C_matching {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("matching.in"));
        scout = new PrintWriter(new File("matching.out"));
        int n = scin.nextInt();
        ArrayList<ArrayList<Integer>> edges = new ArrayList<>(n);
        int[] weights = new int[n];
        for (int i = 0; i < n; i++) {
            edges.add(i, new ArrayList<Integer>());
            weights[i] = scin.nextInt();
        }
        for (int i = 0; i < n; i++) {
            int temp = scin.nextInt();
            for (int j = 0; j < temp; j++) {
                edges.get(i).add(scin.nextInt() - 1);
            }
        }
        boolean[] used = new boolean[n];
        boolean[] weightUsed = new boolean[n];
        int[] matching = new int[n];
        Arrays.fill(matching, -1);
        for (int i = 0; i < n; i++) {
            int maxIndex = -1;
            int maxWeight = 0;
            for (int j = 0; j < n; j++) {
                if (weights[j] > maxWeight && !weightUsed[j]) {
                    maxWeight = weights[j];
                    maxIndex = j;
                }
            }
            weightUsed[maxIndex] = true;
            Arrays.fill(used, false);
            kuhn(maxIndex, used, matching, edges);
        }
        for (int i = 0; i < matching.length; i++) {
            scout.print(matching[i] == -1 ? "0 " : (matching[i] + 1) + " ");
        }
        scout.close();
    }

    private static boolean kuhn(int v, boolean[] used, int[] matching, ArrayList<ArrayList<Integer>> edges) {
        if (used[v]) return false;
        used[v] = true;
        for (int i = 0; i < edges.get(v).size(); i++) {
            int to = edges.get(v).get(i);
            if (matching[to] == -1 || kuhn(matching[to], used, matching, edges)) {
                matching[to] = v;
                return true;
            }
        }
        return false;
    }

    private static FastScanner scin;
    private static PrintWriter scout;

    private static class FastScanner {
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
