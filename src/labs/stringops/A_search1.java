package labs.stringops;
/**
 * @author volhovm
 * Created on 3/15/15
 */

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

public class A_search1 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("search1.in"));
        scout = new PrintWriter(new File("search1.out"));
        String pattern = scin.next();
        String text = scin.next();
        ArrayList<Integer> indexes = new ArrayList<>();
        for (int i = 0; i < text.length() - pattern.length() + 1; i++) {
            for (int j = 0; j < pattern.length(); j++) {
                if (pattern.charAt(j) != text.charAt(i + j)) break;
                if (j == pattern.length() - 1) indexes.add(i);
            }
        }
        scout.println(indexes.size());
        indexes.stream().forEach(i -> scout.print((i + 1) + " "));
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
