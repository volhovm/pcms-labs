package labs.matroids;

import java.io.*;
import java.util.StringTokenizer;

/**
 * @author volhovm
 *         Created on 5/4/15
 */
public class A_schedule {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("schedule.in"));
        scout = new PrintWriter(new File("schedule.out"));

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
