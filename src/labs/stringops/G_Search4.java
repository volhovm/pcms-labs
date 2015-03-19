package labs.stringops;
/**
 * @author volhovm
 * Created on 3/19/15
 */


import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.StringTokenizer;

public class G_Search4 {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("search4.in"));
        scout = new PrintWriter(new File("search4.out"));
        AhoCorasick corasick = new AhoCorasick();
        int n = scin.nextInt();
        ArrayList<String> patterns = new ArrayList<>();
        HashSet<String> found = new HashSet<>();
        for (int i = 0; i < n; i++) {
            patterns.add(scin.next());
        }
        char[] text = scin.next().toCharArray();
        for (String p : patterns) corasick.addPattern(p);
        corasick.setSymlinks();
        AhoCorasick.Node state = corasick.root;
        for (int i = 0; i < text.length && found.size() < patterns.size(); i++) {
            AhoCorasick.Node newState = corasick.delta(state, text[i]);
            AhoCorasick.Node move = newState;
            if (move.isTerminal) found.add(move.terminalHash);
            move = corasick.up(move);
            while (move != null) {
                found.add(move.terminalHash);
                move = corasick.up(move);
            }
            state = newState;
        }
        for (String pattern : patterns) {
            if (found.contains(pattern)) scout.println("YES");
            else scout.println("NO");
        }
        scout.close();
    }

    private static class AhoCorasick {
        static class Node {
            public Node(HashMap<Character, Node> kids, Node symlink, Node parent) {
                this.kids = kids;
                this.symlink = symlink;
                this.parent = parent;
            }

            public Node() {
                kids = new HashMap<>();
            }

            boolean isTerminal;
            HashMap<Character, Node> kids;
            Node symlink, parent;
            String terminalHash;

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

        public AhoCorasick() {
            this.root = new Node(new HashMap<Character, Node>(), root, null);
        }

        public void setSymlinks() {
            setSymlinks(root);
        }

        private void setSymlinks(Node node) {
            if (node.symlink != null) return;
            if (node.parent == null) {
                if (node != root) throw new IllegalStateException();
                node.symlink = node;
            } else {
                if (node.parent == root) node.symlink = root;
                else {
                    char c = '\0';
                    for (char i : node.parent.kids.keySet()) {
                        if (node.parent.kids.get(i) == node) {
                            c = i;
                            break;
                        }
                    }
                    if (node.parent.symlink == null) setSymlinks(node.parent);
                    node.symlink = delta(node.parent.symlink, c);
                }
            }
            for (Node child : node.kids.values()) setSymlinks(child);
        }

        public Node up(Node node) {
            if (node.symlink.isTerminal) return node.symlink;
            if (node.symlink == root) return null;
            return up(node.symlink);
        }

        public void addPattern(String pattern) {
            char[] pt = pattern.toCharArray();
            Node currentNode = root;
            for (int i = 0; i < pt.length; i++) {
                if (!currentNode.kids.containsKey(pt[i]))
                    currentNode.addChild(new Node(), pt[i]);
                currentNode = currentNode.kids.get(pt[i]);
            }
            currentNode.isTerminal = true;
            currentNode.terminalHash = pattern;
        }

        private Node delta(Node nd, char c) {
            if (nd.kids.containsKey(c)) return nd.kids.get(c);
            if (nd == root && !nd.kids.containsKey(c)) return root;
            if (nd.symlink == null) setSymlinks(nd);
            return delta(nd.symlink, c);
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
