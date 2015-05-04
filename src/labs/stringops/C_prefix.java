package labs.stringops;
/**
 * @author volhovm
 * Created on 3/16/15
 */

import java.io.*;
import java.util.StringTokenizer;

public class C_prefix {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("prefix.in"));
        scout = new PrintWriter(new File("prefix.out"));
        String text = scin.next();
        int[] prefix = new int[text.length()];
        prefix[0] = 0;
        for (int i = 1; i < prefix.length; i++) {
            int t = prefix[i - 1];
            while (t > 0 && text.charAt(i) != text.charAt(t)) {
                t = prefix[t - 1];
            }
            if (t > 0) {
                if (text.charAt(i) == text.charAt(t)) {
                    t++;
                }
            } else if (text.charAt(0) == text.charAt(i)) t++;
            else t = 0;
            prefix[i] = t;
        }
        for (int i = 0; i < prefix.length; i++) {
            scout.write(prefix[i] + " ");
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
