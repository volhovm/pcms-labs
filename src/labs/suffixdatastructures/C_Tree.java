package labs.suffixdatastructures;
/**
 * @author volhovm
 * Created on 4/2/15
 */

import java.io.*;
import java.util.HashMap;
import java.util.StringTokenizer;

public class C_Tree {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("tree.in"));
        scout = new PrintWriter(new File("tree.out"));
        char[] src = scin.next().toCharArray();

        scout.close();
    }

    private static class SuffixTree {
        private static class Node {
            public Node(int l, int r, Node parent) {
                this.l = l;
                this.r = r;
                this.parent = parent;
                this.children = new HashMap<>();
                this.symlink = null;
            }

            int l, r;
            Node parent, symlink;
            HashMap<Character, Node> children;

            Node child(Character c) {
                return children.get(c);
            }
        }

        void extendTree(Character c) {
            while (true) {

            }
        }
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
