package labs.stringops;
/**
 * @author volhovm
 * Created on 3/16/15
 */

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class B_search2 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("search2.in"));
        scout = new PrintWriter(new File("search2.out"));
        String pattern = scin.next();
        String text = scin.next();
        if (pattern.length() > text.length()) {
            scout.print("0");
            scout.close();
            return;
        }
        String master = pattern + "#" + text;
        int[] prefix = new int[master.length()];
        prefix[0] = 0;
        for (int i = 1; i < prefix.length; i++) {
            int t = prefix[i - 1];
            while (t > 0 && master.charAt(i) != master.charAt(t)) {
                t = prefix[t - 1];
            }
            if (t > 0 && master.charAt(i) == master.charAt(t) || master.charAt(0) == master.charAt(i)) {
                t++;
            }
            prefix[i] = t;
        }
        ArrayList<Integer> ans = new ArrayList<Integer>(master.length());
        for (int i = pattern.length() * 2; i < master.length(); i++) {
            if (prefix[i] == pattern.length()) ans.add(i - 2 * pattern.length());
        }
        scout.println(ans.size());
        for (int i : ans) {
            scout.print((i + 1) + " ");
        }
        scout.close();
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
