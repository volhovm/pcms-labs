package labs.harderstructures; /**
 *   Created by volhovm on 3/20/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class A_isheap {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("isheap.in"));
        scout = new PrintWriter(new File("isheap.out"));
        int n = scin.nextInt();
        int[] mainArray = new int[n + 1];
        for (int i = 0; i < n; i++) {
            mainArray[i + 1] = scin.nextInt();
        }
        for (int i = 1; i <= n / 2; i++) {
            if (mainArray[i * 2] < mainArray[i] || (i * 2 + 1 < n && mainArray[i * 2 + 1] < mainArray[i])) {
                scout.write("NO");
                scout.close();
                return;
            }
        }
        scout.write("YES");
        scout.close();
    }

    public static FastScanner scin;
    public static PrintWriter scout;

    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        double nextDouble() {
            return Double.parseDouble(next());
        }
    }
}
