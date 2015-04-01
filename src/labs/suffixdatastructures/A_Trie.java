package labs.suffixdatastructures;
/**
 * @author volhovm
 * Created on 4/1/15
 */

import java.io.*;
import java.util.HashMap;
import java.util.StringTokenizer;

public class A_Trie {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("trie.in"));
        scout = new PrintWriter(new File("trie.out"));
        Trie trie = new Trie();
        StringBuilder in = new StringBuilder(scin.next());
        for (int i = 0; i < in.length(); i++) {
            trie.addPattern(in.substring(i, in.length()));
        }
        trie.dump(scout);
        scout.close();
    }

    private static class Trie {
        static class Node {
            public Node(HashMap<Character, Node> kids, Node parent, int num) {
                this.kids = kids;
                this.parent = parent;
                number = num;
            }

            public Node(int num) {
                kids = new HashMap<>();
                number = num;
            }

            boolean isTerminal;
            HashMap<Character, Node> kids;
            int number;
            Node parent;

            void addChild(Node child, char ch) {
                this.kids.put(ch, child);
                child.parent = this;
            }

            @Override
            public String toString() {
                return kids.toString();
            }
        }

        Node root;
        int size = 1;

        public Trie() {
            this.root = new Node(new HashMap<Character, Node>(), null, 1);
        }

        public void addPattern(String pattern) {
            char[] pt = pattern.toCharArray();
            Node currentNode = root;
            for (char aPt : pt) {
                if (!currentNode.kids.containsKey(aPt)) {
                    currentNode.addChild(new Node(size + 1), aPt);
                    size++;
                }
                currentNode = currentNode.kids.get(aPt);
            }
            currentNode.isTerminal = true;
        }

        void dump(PrintWriter writer) {
            writer.println(size + " " + (size - 1));
            dumpNode(root, writer);
        }

        private void dumpNode(Node node, PrintWriter writer) {
            for (Character ch : node.kids.keySet()) {
                writer.println(node.number + " " + node.kids.get(ch).number + " " + ch);
                dumpNode(node.kids.get(ch), writer);
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
