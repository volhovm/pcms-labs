package labs.sortsearch;

/**
 * @author volhovm
 * Created on 5/24/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class F_antiQS {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("antiqs.in"));
        scout = new PrintWriter(new File("antiqs.out"));
        int a = scin.nextInt();
        int[] arr = new int[a];
        for (int i = 0; i < a; i++){
            arr[i] = i + 1;
        }
        for (int i = 2; i < a; i++){
            int temp = arr[i];
            arr[i] = arr[i / 2];
            arr[i / 2] = temp;
        }
        for (int x : arr){
            scout.print(x + " ");
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
