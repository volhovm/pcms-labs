package labs.suffixdatastructures;
/**
 * @author volhovm
 * Created on 4/8/15
 */

import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;

public class G_common {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("common.in"));
        scout = new PrintWriter(new File("common.out"));
        String part1 = scin.next();
        String part2 = scin.next();
        char src[] = (part1 + "`" + part2 + ((char)(((int) '`') - 1))).toCharArray();
        int divider = part1.length();
        int[] p = suffArr(src);
        int[] lcp = suffArrToLcp(src, p);
        int max = 0;
        int maxi = -1;
        for (int i = 0; i < lcp.length - 1; i++) {
            if (p[i+1] > divider && p[i] < divider ||
                    p[i+1] < divider && p[i] > divider) {
                int currmax = lcp[i];
                if (currmax > max) {
                    max = currmax;
                    maxi = p[i];
                }
            }
        }
        for (int i = 0; i < max; i++) {
            scout.print(src[maxi + i]);
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

    private static int[] suffArrToLcp(char[] src, int[] p) {
        int[] lcp = new int[src.length];
        int[] rev = new int[src.length];
        for (int i = 0; i < src.length; i++) rev[p[i]] = i;
        int counter = 0;
        for (int i = 0; i < src.length; i++) {
            if (counter > 0) counter--;
            if (rev[i] == src.length - 1) {
                counter = 0;
                lcp[src.length - 1] = -1;
            } else {
                int temp = p[rev[i] + 1];
                while (Math.max(i + counter, temp + counter) < src.length
                        && src[i + counter] == src[temp + counter])
                    counter++;
                lcp[rev[i]] = counter;
            }
        }
        return lcp;
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
