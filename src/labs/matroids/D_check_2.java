package labs.matroids;

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * @author volhovm
 *         Created on 5/5/15
 */
public class D_check_2 {
    private static boolean sameContent(ArrayList<Integer> a, ArrayList<Integer> b) {
        for (Integer anA : a) {
            if (!b.contains(anA)) return false;
        }
        return true;
    }

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("check.in"));
        scout = new PrintWriter(new File("check.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        ArrayList<ArrayList<Integer>> s = new ArrayList<>(m);
        for (int i = 0; i < m; i++) {
            int temp = scin.nextInt();
            s.add(i, new ArrayList<>(temp));
            for (int j = 0; j < temp; j++) {
                s.get(i).add(j, scin.nextInt());
            }
        }
        // emptyset check
        for (int i = 0; i < m; i++) {
            if (s.get(i).size() == 0) break;
            if (i == m - 1) {
                scout.print("NO");
                return;
            }
        }
        // 2nd axiom check
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < s.get(i).size(); j++) {

            }
        }
        scout.close();
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
