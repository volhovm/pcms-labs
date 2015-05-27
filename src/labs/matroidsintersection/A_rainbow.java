package labs.matroidsintersection;

import java.io.*;
import java.util.*;

/**
 * Doesn't work, TL7
 * <p>
 * TODO Remove used -- DONE
 * TODO Rewrite DSU without Akkerman -- DONE
 * TODO Remove all new -- DONE
 * TODO Outline all new out of loops -- DONE
 * WTF
 *
 * @author volhovm
 *         Created on 5/24/15
 */
public class A_rainbow {
    private FastScanner scin;
    private PrintWriter scout;
    long start = System.currentTimeMillis();

    public long track(String tag) {
        long newStart = System.currentTimeMillis();
        long delta = (newStart - start);
        start = newStart;
//        System.out.println(tag + ": " + delta);
        return delta;
    }

    void start() throws IOException {
        track("start");
        scin = new FastScanner(new File("rainbow.in"));
        scout = new PrintWriter(new File("rainbow.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        double start = System.currentTimeMillis();
        int[] edgeFrom = new int[m];
        int[] edgeTo = new int[m];
        int[] edgeColor = new int[m];
        for (int i = 0; i < m; i++) {
            edgeFrom[i] = scin.nextInt() - 1;
            edgeTo[i] = scin.nextInt() - 1;
            edgeColor[i] = scin.nextInt() - 1;
        }

        long preinit = 0, rl = 0, x1 = 0, x2 = 0, bfs = 0, endloop = 0;

        System.out.println("read: " + track("read"));

        boolean[] set = new boolean[m];
        boolean[] colorUsed = new boolean[105];
        DSU dsu = new DSU(n);
        ArrayList<Integer> actualSet = new ArrayList<>(n);
        ArrayList<Integer> otherSet = new ArrayList<>(m);
        ArrayList<Integer> X1 = new ArrayList<>();
        boolean[] X2 = new boolean[m];
        List<Integer>[] exchange = new List[m];
        Arrays.setAll(exchange, i -> new ArrayList<>(m));
        Queue<Integer> queue = new ArrayDeque<>();
        int[] ancestor = new int[m];

        System.out.println("init: " + track("init"));
        set[0] = true;
        colorUsed[edgeColor[0]] = true;
        outer:
        while (true) {
            actualSet.clear();
            otherSet.clear();
            for (int i = 0; i < set.length; i++)
                if (set[i]) actualSet.add(i);
                else otherSet.add(i);
            // rebuild dsu for naive adding
            dsu.reset();
            for (int elem : actualSet) {
                dsu.union(edgeFrom[elem], edgeTo[elem]);
            }
            // add vertices naively
            for (int elem : otherSet) {
                if (dsu.get(edgeFrom[elem]) != dsu.get(edgeTo[elem]) && !colorUsed[edgeColor[elem]]) {
                    dsu.union(edgeFrom[elem], edgeTo[elem]);
                    colorUsed[edgeColor[elem]] = true;
                    set[elem] = true;
                    continue outer;
                }
            }
            Arrays.stream(exchange).forEach(List::clear);
            preinit += track("preinit");

            // left to right && right to left
            for (int i : actualSet) {
                Arrays.fill(colorUsed, false);
                dsu.reset();
                for (int elem : actualSet) {
                    if (elem != i) {
                        dsu.union(edgeFrom[elem], edgeTo[elem]);
                        colorUsed[edgeColor[elem]] = true;
                    }
                }
                // for all in X \ S check if can be in S
                for (int j : otherSet) {
                    if (dsu.get(edgeFrom[j]) != dsu.get(edgeTo[j])) {
                        exchange[i].add(j);
                    }
                    if (!colorUsed[edgeColor[j]]) {
                        exchange[j].add(i);
                    }
                }
            }

            rl += track("right->left && backwards");

            // Creating X1
            X1.clear();
            dsu.reset();
            for (int elem : actualSet) dsu.union(edgeFrom[elem], edgeTo[elem]);
            for (int i : otherSet) {
                if (dsu.get(edgeFrom[i]) != dsu.get(edgeTo[i])) X1.add(i);
            }

            x1 += track("X1");

            // Creating X2
            Arrays.fill(X2, false);
            Arrays.fill(colorUsed, false);
            for (int elem : actualSet) colorUsed[edgeColor[elem]] = true;
            for (int i : otherSet) {
                if (!set[i] && !colorUsed[edgeColor[i]]) X2[i] = true;
            }

            x2 += track("X2");

            // check if X1 and X2 intersect
            for (int elem : X1) {
                if (X2[elem]) {
                    set[elem] ^= true;
                    continue outer;
                }
            }
//
            track("quickcheck");

            int endVertex = -1;
            Arrays.fill(ancestor, -1);
            queue.clear();
            queue.addAll(X1);
            queue.forEach(i -> ancestor[i] = i);
            while (!queue.isEmpty()) {
                int curr = queue.poll();
                if (X2[curr]) {
                    endVertex = curr;
                    break;
                }
                for (int child : exchange[curr]) {
                    if (ancestor[child] == -1) {
                        ancestor[child] = curr;
                        queue.add(child);
                    }
                }
            }

            bfs += track("bfs");

//            System.out.println("after bfs " + System.currentTimeMillis());
            if (endVertex == -1) {
                break outer;
            }
            for (; ; endVertex = ancestor[endVertex]) {
                System.out.println(endVertex);
                set[endVertex] ^= true; // remove if present, add if not present
                colorUsed[edgeColor[endVertex]] ^= true;
                if (endVertex == ancestor[endVertex]) break;
            }
            endloop += track("endloop");
        }

        actualSet.clear();
        System.out.println("preinit: " + preinit);
        System.out.println("r->l->r: " + rl);
        System.out.println("x1:      " + x1);
        System.out.println("x2:      " + x2);
        System.out.println("bfs:     " + bfs);
        System.out.println("end:     " + endloop);
        for (int i = 0; i < set.length; i++) if (set[i]) actualSet.add(i);
        scout.println(actualSet.size());
        for (int elem : actualSet) scout.print((elem + 1) + " ");
        scout.close();
    }
//
//    private static class DSU {
//        private int[] array;
//
//        public DSU(int size) {
//            this.array = new int[size];
//            reset();
//        }
//
//        public void reset() {
//            Arrays.setAll(array, i -> i);
//        }
//
//        int get(int index) {
//            return array[index] == index ? index : get(array[index]);
//        }
//
//        void union(int a, int b) {
//            int x = get(a);
//            int y = get(b);
//            array[y] = x;
//        }
//    }

    private class DSU {
        DSUNode[] nodes;

        public DSU(int n) {
            nodes = new DSUNode[n];
            for (int i = 0; i < nodes.length; i++) {
                nodes[i] = new DSUNode(i, i, 1, -1);
            }
        }

        public void reset() {
            for (int i = 0; i < nodes.length; i++) {
                nodes[i] = new DSUNode(i, i, 1, -1);
                nodes[i].min = i;
                nodes[i].max = i;
                nodes[i].count = 1;
                nodes[i].parentIndex = -1;
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

    public static void main(String[] args) throws IOException, InterruptedException {
        A_rainbow task = new A_rainbow();
        task.start();
    }

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
    }
}
