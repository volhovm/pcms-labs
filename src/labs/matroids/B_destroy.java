package labs.matroids;

import java.io.*;
import java.util.*;

/**
 * @author volhovm
 *         Created on 5/4/15
 */
public class B_destroy {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("destroy.in"));
        scout = new PrintWriter(new File("destroy.out"));
        List<Edge> edges = new LinkedList<>();
        int n = scin.nextInt();
        int m = scin.nextInt();
        long s = scin.nextLong();
        for (int i = 0; i < m; i++) {
            edges.add(new Edge(scin.nextInt() - 1, scin.nextInt() - 1, scin.nextLong(), i + 1));
        }
        Collections.sort(edges, (a, b) -> Long.compare(b.weight, a.weight));
        DSU dsu = new DSU(n);
        List<Edge> usedEdges = new ArrayList<>();
        List<Edge> removedEdges = new ArrayList<>();
        while (!edges.isEmpty()) {
            Edge e = edges.get(0);
            if (dsu.get(e.from) != dsu.get(e.to)) {
                dsu.union(e.from, e.to);
                usedEdges.add(e);
                edges.remove(e);
            } else {
                removedEdges.add(e);
                edges.remove(0);
            }
        }
        long sum = 0;
        for (Edge removedEdge : removedEdges) {
            sum += removedEdge.weight;
        }
        Collections.sort(removedEdges, (a, b) -> Long.compare(a.weight, b.weight));
        while (sum > s && !removedEdges.isEmpty()) {
            Edge e = removedEdges.get(removedEdges.size() - 1);
            sum -= e.weight;
            removedEdges.remove(removedEdges.size() - 1);
        }
        scout.println(removedEdges.size());
        for (Edge removedEdge : removedEdges) {
            scout.print(removedEdge.id + " ");
        }
        scout.close();
    }

    private static class Edge {
        int from, to, id;
        long weight;

        public Edge(int from, int to, long weight, int id) {
            this.from = from;
            this.to = to;
            this.weight = weight;
            this.id = id;
        }

        @Override
        public String toString() {
            return from + "---{" + weight + "}---" + to;
        }
    }

    private static class DSU {
        DSUNode[] nodes;

        public DSU(int n) {
            nodes = new DSUNode[n];
            for (int i = 0; i < n; i++) {
                nodes[i] = new DSUNode(i, i, 1, -1);
            }
        }

        public void printAllNodeData(int index, PrintWriter printWriter) {
            DSUNode classRepresentative = nodes[get(index)];
            printWriter.println(classRepresentative.min + " " + classRepresentative.max + " " + classRepresentative.count);
        }

        public int get(int index) {
            if (nodes[index].parentIndex == -1) {
                return index;
            } else {
                int current = get(nodes[index].parentIndex);
                nodes[index].parentIndex = current;
                return current;
            }
        }

        public void union(int a, int b) {
            int aFatherIndex = get(a);
            int bFatherIndex = get(b);
            if (aFatherIndex == bFatherIndex) return;
            DSUNode aFather = nodes[aFatherIndex];
            DSUNode bFather = nodes[bFatherIndex];
            if (aFather.count > bFather.count) {
                bFather.parentIndex = aFatherIndex;
                aFather.min = StrictMath.min(aFather.min, bFather.min);
                aFather.max = StrictMath.max(aFather.max, bFather.max);
                aFather.count += bFather.count;
            } else {
                aFather.parentIndex = bFatherIndex;
                bFather.min = StrictMath.min(aFather.min, bFather.min);
                bFather.max = StrictMath.max(aFather.max, bFather.max);
                bFather.count += aFather.count;
            }
        }

        private class DSUNode {
            int min, max, count, parentIndex;

            private DSUNode(int min, int max, int count, int parentIndex) {
                this.min = min;
                this.max = max;
                this.count = count;
                this.parentIndex = parentIndex;
            }
        }
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

        long nextLong() throws IOException {
            return Long.parseLong(next());
        }
    }
}
