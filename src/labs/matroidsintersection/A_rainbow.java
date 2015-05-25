package labs.matroidsintersection;

import com.sun.org.apache.bcel.internal.generic.ARRAYLENGTH;

import java.io.*;
import java.lang.reflect.Array;
import java.util.*;

/**
 * Doesn't work, TL7
 * @author volhovm
 *         Created on 5/24/15
 */
public class A_rainbow {
    public static void main(String[] args) throws IOException, InterruptedException {
        scin = new FastScanner(new File("rainbow.in"));
        scout = new PrintWriter(new File("rainbow.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        // vertex -> color
        Edge[] edges = new Edge[m];
        for (int i = 0; i < m; i++) {
            int x = scin.nextInt() - 1;
            int y = scin.nextInt() - 1;
            int color = scin.nextInt() - 1;
            edges[i] = new Edge(x, y, color, i);
        }
        boolean maxReached = false;
        Set<Integer> set = new HashSet<>();
//        Collection<Integer> set = new TreeSet<>(); // maxn = 100
//        IntSet set = new IntSet(110); // 110 > maxn
        DSU setDsu = new DSU(m);
        boolean resetDsu = true;
        set.add(0);
        while (!maxReached) {
            ArrayList<ArrayList<Integer>> exchange = new ArrayList<>(m);
            for (int i = 0; i < m; i++) exchange.add(new ArrayList<>());
            // iterate over all in S add if set's ok
            DSU tempDsu = new DSU(n);
            for (int i : set) {
                for (int elem : set) {
                    if (elem != i) tempDsu.union(edges[elem].from, edges[elem].to);
                }
                // for all in X \ S check if can be in S
                for (int j = 0; j < m; j++) {
                    if (!set.contains(j)) {
                        if (tempDsu.get(edges[j].from) != tempDsu.get(edges[j].to)) {
                            exchange.get(i).add(j);
                        }
                    }
                }
                tempDsu.reset();
            }
            // iterate over all in S
            {
                boolean[] colorUsed = new boolean[110];
                for (int i : set) {
                    for (int elem : set) {
                        if (elem != i) colorUsed[edges[elem].color] = true;
                    }
                    // for all in X \ S check if can be in S instead of i
                    for (int j = 0; j < m; j++) {
                        if (!set.contains(j)) {
                            if (!colorUsed[edges[j].color]) {
                                exchange.get(j).add(i);
                            }
                        }
                    }
                    Arrays.fill(colorUsed, false);
                }
            }
            // Creating X2
            Set<Integer> X2 = new HashSet<>();
            {
                boolean[] colorsUsed = new boolean[110];
                for (int elem : set) colorsUsed[edges[elem].color] = true;
                for (int i = 0; i < m; i++) {
                    if (!set.contains(i) && !colorsUsed[edges[i].color]) X2.add(i);
                }
            }

            // Creating X1
            Set<Integer> X1 = new HashSet<>();
            {
                if (resetDsu) {
                    setDsu.reset();
                    for (int elem : set) setDsu.union(edges[elem].from, edges[elem].to);
                    resetDsu = false;
                }
                for (int i = 0; i < m; i++) {
                    if (!set.contains(i) && setDsu.get(edges[i].from) != setDsu.get(edges[i].to)) X1.add(i);
                }
            }

//            System.out.println("bfs " + System.currentTimeMillis());
            int endVertex = -1;
            int[] ancestors = new int[m];
            Arrays.fill(ancestors, -1);
            ArrayDeque<Integer> queue = new ArrayDeque<>();
            boolean[] used = new boolean[m];
            X1.stream().forEach(queue::addLast);
            X1.stream().forEach(i -> used[i] = true);
            while (!queue.isEmpty()) {
                int curr = queue.removeFirst();
                if (X2.contains(curr)) {
                    // do something cool
                    endVertex = curr;
                    break;
                }
                for (int child : exchange.get(curr)) {
                    if (!used[child]) {
                        used[child] = true;
                        ancestors[child] = curr;
                        queue.addLast(child);
                    }
                }
            }
//            System.out.println("after bfs " + System.currentTimeMillis());
            if (endVertex == -1) {
                maxReached = true;
                continue;
            }
            for (; endVertex != -1; endVertex = ancestors[endVertex]) {
//                System.out.println("mda test " + endVertex);
                if (set.contains(endVertex)) {
                    set.remove(endVertex);
                    resetDsu = true;
                } else {
                    set.add(endVertex);
                    if (!resetDsu) setDsu.union(edges[endVertex].from, edges[endVertex].to);
                }
            }
            System.out.println("test " + System.currentTimeMillis());
        }

        scout.println(set.size());
        for (int elem : set) scout.print((elem + 1) + " ");
        scout.close();
    }

    private static class IntSet implements Iterable<Integer> {
        private int[] array;
        private int size;
        int last = 0;

        public IntSet(int n) {
            this.array = new int[n];
            this.size = 0;
            Arrays.fill(array, -1);
        }

        public void add(int value) {
            while (last < array.length && array[last] != -1) last++;
            array[last++] = value;
            size++;
        }

        public void remove(int value) {
            for (int i = 0; i < array.length; i++) {
                if (array[i] == value) {
                    array[i] = -1;
                    last = i;
                    size--;
                    break;
                }
            }
        }

        public boolean contains(int value) {
            for (int i = 0; i < array.length; i++) {
                if (array[i] == value) return true;
            }
            return false;
        }

        @Override
        public Iterator<Integer> iterator() {
            return new Iterator<Integer>() {
                private int pos = 0;

                @Override
                public boolean hasNext() {
                    return pos < array.length;
                }

                @Override
                public Integer next() {
                    int res = array[pos++];
                    while (pos < array.length && array[pos] == -1) pos++;
                    return res;
                }
            };
        }

        public int size() {
            return size;
        }
    }

    private static class Edge {
        int from, to, color, id;

        public Edge(int from, int to, int color, int id) {
            this.from = from;
            this.to = to;
            this.color = color;
            this.id = id;
        }
    }

    private static class DSU {
        DSUNode[] nodes;

        public DSU(int n) {
            nodes = new DSUNode[n];
            reset();
        }

        public void reset() {
            for (int i = 0; i < nodes.length; i++) {
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
    }
}
