package labs.suffixdatastructures;

import java.io.*;
import java.util.*;
import java.util.stream.Collector;

/**
 * @author volhovm
 *         Created on 4/9/15
 */
// There's something java8-specific in here!
public class D_automaton {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("automaton.in"));
        scout = new PrintWriter(new File("automaton.out"));
        SufAutomaton automation = new SufAutomaton();
        char[] src = scin.next().toCharArray();
        for (char c : src) {
            automation.addNode(c);
//            automation.dump(scout);
        }
        automation.setTerminalMarks();
        automation.dump(scout);
        scout.close();
    }

    private static class SufAutomaton {
        static class State {
            public State(int length, int id, State sufLink) {
                this(length, id, sufLink, new HashMap<>());
            }

            public State(int length, int id, State sufLink, HashMap<Character, State> children) {
                this.length = length;
                this.id = id;
                this.sufLink = sufLink;
                this.children = children;
            }

            int length, id;
            boolean isTerminal;
            State sufLink;
            HashMap<Character, State> children;

            @Override
            public String toString() {
                return Integer.toString(this.id) + "->" + children;
            }
        }

        public SufAutomaton() {
            this.root = new State(0, 1, null);
            this.last = root;
            this.vsize = 1;
        }

        private State root, last;
        int vsize;

        void addNode(char c) {
            State current = new State(last.length + 1, ++vsize, null);
            State p = last;
            // add edges values
            for (; p != null && !p.children.containsKey(c); p = p.sufLink) {
                p.children.put(c, current);
            }
            // set suflinks
            if (p == null) current.sufLink = root;
            else {
                State q = p.children.get(c);
                if (p.length + 1 == q.length) {
                    current.sufLink = q;
                } else {
                    State clone = new State(p.length + 1, ++vsize, q.sufLink,
                            new HashMap<Character, State>(q.children));
//                    p = p.sufLink;
                    for (; p != null && p.children.get(c) == q; p = p.sufLink) {
                        p.children.put(c, clone);
                    }
                    q.sufLink = clone;
                    current.sufLink = clone;
                }
            }
            last = current;
        }

        void setTerminalMarks() {
            State p = last;
            for (; p != null
//                    && p != root
                    ; p = p.sufLink) {
                p.isTerminal = true;
            }
        }

        void dump(PrintWriter writer) {
            StringBuilder middle = new StringBuilder();
            int esize = 0;
            ArrayList<Integer> terminals = new ArrayList<>();
            ArrayDeque<State> deque = new ArrayDeque<>();
            HashSet<State> visited = new HashSet<>();
            deque.addLast(root);
            while (!deque.isEmpty()) {
                State curState = deque.pollFirst();
                if (curState.isTerminal) terminals.add(curState.id);
                for (Map.Entry<Character, State> x : curState.children.entrySet()) {
                    if (!visited.contains(x.getValue())) {
                        visited.add(x.getValue());
                        deque.addLast(x.getValue());
                    }
                    esize++;
                    middle.append(curState.id).append(" ")
                            .append(x.getValue().id).append(" ")
                            .append(x.getKey()).append("\n");
                }
            }
            writer.printf("%d %d\n", vsize, esize);
            writer.printf("%s", middle.toString());
            writer.printf("%d\n", terminals.size());
            for (Integer i : terminals) writer.printf("%d ", i);
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
