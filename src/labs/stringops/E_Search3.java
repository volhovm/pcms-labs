package labs.stringops;
/**
 * @author volhovm
 * Created on 3/16/15
 */

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class E_Search3 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("search3.in"));
        scout = new PrintWriter(new File("search3.out"));
        String pattern = scin.next();
        int n = pattern.length();
        String text = scin.next();
        if (n > text.length()) {
            scout.write("0");
            scout.close();
            return;
        }
        String master = pattern + "#" + text;
        String masterRev = new StringBuilder(pattern).reverse() + "#" + new StringBuilder(text).reverse();
        int[] zf = zFunction(master);
        int[] zfRev = zFunction(masterRev);
        ArrayList<Integer> ans = new ArrayList<Integer>(master.length());
        for (int i = 0; i < text.length() - n + 1; i++) {
            int zSum = zf[n + i + 1] + zfRev[master.length() - i - n];
            if (zSum > n - 2) ans.add(i + 1);
        }
        scout.println(ans.size());
        for (int i : ans) {
            scout.print(i + " ");
        }
        scout.close();
    }

    public static int[] zFunction(String string) {
        int[] zf = new int[string.length()];
        int l = 0, r = 0;
        for (int i = 1; i < string.length(); i++) {
            zf[i] = Math.max(0, Math.min(r - i, zf[i - l]));
            while (i + zf[i] < string.length() && string.charAt(zf[i]) == string.charAt(i + zf[i])) {
                zf[i]++;
            }
            if (i + zf[i] >= r) {
                l = i;
                r = i + zf[i];
            }
        }
        return zf;
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
