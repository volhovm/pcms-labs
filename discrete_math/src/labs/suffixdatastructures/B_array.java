package labs.suffixdatastructures;
/**
 * @author volhovm
 * Created on 4/1/15
 */

import java.io.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.StringTokenizer;

public class B_array {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("array.in"));
        scout = new PrintWriter(new File("array.out"));
        char[] src = (scin.next() + "_").toCharArray();
        int[] p = suffArr(src);
        for (int i = 1; i < src.length; i++) {
            scout.print((p[i] + 1) + " ");
        }
        scout.close();
    }

    private static int[] suffArr(char[] src) {
        int n = src.length;
        int[] p = new int[n];   // for permutation
        int[] c = new int[n];   // for pairs
        int[] cnt = new int[Math.max(n, 28)]; // for counting

        // counting sort
        for (char aSrc : src) ++cnt[aSrc - 'a' + 2];
        for (int i = 1; i < 28 && i < cnt.length; i++) // 28 for safety!
            cnt[i] += cnt[i - 1];
        // writeback
        for (int i = 0; i < n; i++) p[--cnt[src[i] - 'a' + 2]] = i;
        c[p[0]] = 0;
        int classes = 1; // classes of equality
        for (int i = 1; i < n; i++) {
            if (src[p[i]] != src[p[i - 1]]) classes++;
            c[p[i]] = classes - 1;
        }

        // new p, c
        int[] pn = new int[n];
        int[] cn = new int[n];
        for (int j = 0; (1 << j) < n; j++) {
            for (int i = 0; i < n; i++) {
                pn[i] = p[i] - (1 << j);
                if (pn[i] < 0) pn[i] += n;
            }
            // countsort again
            Arrays.fill(cnt, 0);
            for (int i = 0; i < n; i++) cnt[c[pn[i]]]++;
            for (int i = 1; i < classes; i++) cnt[i] += cnt[i - 1];
            for (int i = n - 1; i >= 0; i--) p[--cnt[c[pn[i]]]] = pn[i];

            // count recount (heh)
            cn[p[0]] = 0;
            classes = 1;
            for (int i = 1; i < n; i++) {
                if (c[p[i]] != c[p[i - 1]] ||
                        c[(p[i] + (1 << j)) % n] != c[(p[i - 1] + (1 << j)) % n]) classes++;
                cn[p[i]] = classes - 1;
            }
            System.arraycopy(cn, 0, c, 0, n);
        }
        return p;
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
