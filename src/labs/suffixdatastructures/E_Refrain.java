package labs.suffixdatastructures;
/**
 * @author volhovm
 * Created on 4/8/15
 */

import java.io.*;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashMap;
import java.util.StringTokenizer;

public class E_Refrain {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("refrain.in"));
        scout = new PrintWriter(new File("refrain.out"));
        StringBuilder stringBuilder = new StringBuilder();
        int n = scin.nextInt();
        int m = scin.nextInt();
        for (int i = 0; i < n; i++) {
            stringBuilder.append(((char) scin.nextInt()));
        }
        n++;
        stringBuilder.append((char) 0);
        char[] src = stringBuilder.toString().toCharArray();
        int[] p = suffArr(src);
        int[] lcp = suffArrToLcp(src, p);
//        lcp[n - 1] = 0;

        class Tuple3 {
            int width, id, height;

            public Tuple3(int width, int id, int height) {
                this.width = width;
                this.id = id;
                this.height = height;
            }
        }

        int maxWidth = 1;
        int maxHeight = n - 1;
        int start = 0;
        ArrayDeque<Tuple3> stack = new ArrayDeque<>();

        for (int i = 1; i < n; i++) {
            int curr = 1;
            while (!stack.isEmpty() && lcp[i] < stack.peek().height + 1) {
                Tuple3 temp = stack.pop();
                curr += temp.width;
                if (((long) maxWidth) * ((long) maxHeight) < ((long) curr) * ((long) temp.height)) {
                    start = p[temp.id];
                    maxWidth = curr;
                    maxHeight = temp.height;
                }
            }
            if (stack.isEmpty() || stack.peek().height < lcp[i]) {
                stack.push(new Tuple3(curr, i, lcp[i]));
            }
        }
        scout.println(((long) maxWidth * ((long) maxHeight)));
        scout.println(maxHeight);
        for (int i = start; i < start + maxHeight; i++) {
            scout.print((int) src[i]);
            scout.print(" ");
        }
        scout.close();
    }

    private static int[] suffArr(char[] src) {
        int n = src.length;
        int[] p = new int[n];   // for permutation
        int[] c = new int[n];   // for pairs
        int[] cnt = new int[Math.max(n, 12)]; // for counting

        // counting sort
        for (char aSrc : src) ++cnt[aSrc];
        for (int i = 1; i < 12 && i < cnt.length; i++)
            cnt[i] += cnt[i - 1];
        // writeback
        for (int i = 0; i < n; i++) p[--cnt[src[i]]] = i;
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
