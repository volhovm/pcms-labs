package labs.trees;
/**
 * @author volhovm
 *         Created on 08.04.14
 */

import libraries.SplayTree;

import java.io.*;
import java.util.StringTokenizer;

public class C_splayMTF {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("movetofront.in"));
        scout = new PrintWriter(new File("movetofront.out"));
        SplayTree splayTree = new SplayTree();
        int n = scin.nextInt();
        int m = scin.nextInt();
//        splayTree.add(n / 2, n / 2 - 1);
//        for (int i = n / 2 - 1; i >= 0; i--) {
//            splayTree.add(i, i - 1);
//            splayTree.add(n / 2 + i, n - i + 1);
//        }
        for (int i = 0; i < n; i++) {
            splayTree.add(i + 1, i);
        }
        for (int i = 0; i < m; i++) {
            splayTree.reconstruct(scin.nextInt(), scin.nextInt());
        }
        scout.println(splayTree.listAll().trim());
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
