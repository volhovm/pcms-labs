package labs.stringops;
/**
 * @author volhovm
 * Created on 3/16/15
 */

import java.io.*;
import java.util.StringTokenizer;

public class F_period {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("period.in"));
        scout = new PrintWriter(new File("period.out"));
        String text = scin.next();
        int[] zf = zFunction(text);
        int ans = zf.length;
        for (int i = 0; i < text.length() / 2 + 1; i++) {
            if (zf[i] == text.length() - i) {
                ans = i;
                break;
            }
        }
        scout.print(ans);
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
