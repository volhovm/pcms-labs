package labs.stringops;
/**
 * @author volhovm
 * Created on 3/16/15
 */


import java.io.*;
import java.util.StringTokenizer;

public class D_z {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("z.in"));
        scout = new PrintWriter(new File("z.out"));
        String text = scin.next();
        int[] zf = zFunction(text);
        for (int i = 1; i < zf.length; i++) {
            scout.write(zf[i] + " ");
        }
        scout.close();
    }

    private static int min(int a, int b) {
        return a < b ? a : b;
    }

    private static int max(int a, int b) {
        return a > b ? a : b;
    }

    private static int[] zFunction(String string) {
        int[] zf = new int[string.length()];
        int l = 0, r = 0;
        for (int i = 1; i < string.length(); i++) {
            zf[i] = max(0, min(r - i, zf[i - l]));
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
