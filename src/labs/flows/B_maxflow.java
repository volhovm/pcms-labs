package labs.flows;
/**
 * @author volhovm
 * Created on 11/23/14
 */

import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;


//it gets TL33 (see the cpp version)
public class B_maxflow {
    private static final int INF = Integer.MAX_VALUE;
    private static int n;
    private static int t;
    private static int s = 0;
    private static int[] dist;
    private static int[][] c, f;

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("maxflow.in"));
        scout = new PrintWriter(new File("maxflow.out"));
        n = scin.nextInt();
        t = n - 1;
        dist = new int[n];
        c = new int[n][n];
        f = new int[n][n];
        int m = scin.nextInt();
        for (int i = 0; i < m; i++) {
            int a = scin.nextInt() - 1;
            int b = scin.nextInt() - 1;
            c[a][b] = scin.nextInt();
        }
        int flow = 0;
        while (true) {
            if (!bfs()) break;
            int pushed;
            while (true) {
                pushed = dfs(s, INF);
                if (pushed == 0) break;
                flow += pushed;
            }
        }
        scout.print(flow);
        scout.close();
    }

    static boolean bfs() {
        int[] q = new int[n];
        int c1 = 0;
        int c2 = 0;
        q[c2++] = s;
        Arrays.fill(dist, -1);
        dist[s] = 0;
        while (c1 < c2) {
            int v = q[c1++];
            for (int i = 0; i < n; ++i)
                if (dist[i] == -1 && f[v][i] < c[v][i]) {
                    q[c2++] = i;
                    dist[i] = dist[v] + 1;
                }
        }
        return dist[t] != -1;
    }

    static int dfs(int v, int flow) {
        if (flow == -1) return 0;
        if (v == t) return flow;
        for (int to = 0; to < n; ++to) {
            if (dist[to] != dist[v] + 1) continue;
            int a = c[v][to] - f[v][to];
            int pushed = dfs(to, (a < flow) ? a : flow);
            if (pushed != 0) {
                f[v][to] += pushed;
                f[to][v] -= pushed;
                return pushed;
            }
        }
        return 0;
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
