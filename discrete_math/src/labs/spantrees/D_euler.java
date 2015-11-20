package labs.spantrees;/**
 * @author volhovm
 * Created on 11/6/14
 */


import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class D_euler {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("euler.in"));
        scout = new PrintWriter(new File("euler.out"));
        int n = scin.nextInt();
        int[][] g = new int[n][n];
        ArrayList<Integer> odd = new ArrayList<>();
        ArrayList<Integer> path = new ArrayList<>();
        int[] outVertexes = new int[n];
        for (int i = 0; i < n; i++) {
            int m = scin.nextInt();
            if (m % 2 == 1) odd.add(i);
            for (int j = 0; j < m; j++) {
                g[i][scin.nextInt() - 1]++;
                outVertexes[i]++;
            }
        }
        if (odd.size() > 2) {
            scout.print(-1);
        } else {
            LinkedList<Integer> stack = new LinkedList<>();
            stack.push(odd.size() > 0 ? odd.get(0) : 0);
            while (!stack.isEmpty()){
                int v = stack.peek();
                if (outVertexes[v] == 0) {
                    path.add(v);
                    stack.pop();
                } else {
                    // select one vertex, delete edge to it, push U
                    for (int u = 0; u < n; u++) {
                        if (g[v][u] > 0) {
                            stack.push(u);
                            // deleting vertexes
                            g[v][u]--;
                            outVertexes[u]--;
                            g[u][v]--;
                            outVertexes[v]--;
                            break;
                        }
                    }
                }
            }
            scout.println(path.size() - 1);
            for (Integer aPath : path) {
                scout.print((aPath + 1) + " ");
            }
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
