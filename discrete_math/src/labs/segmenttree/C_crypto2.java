package labs.segmenttree;

import java.util.*;
import java.io.*;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class C_crypto2 {
    private static double start = System.currentTimeMillis();
    private static void checkStamp(String msg){
        System.out.println(msg + " " + (System.currentTimeMillis() - start));
        start = System.currentTimeMillis();
    }

    FastScanner in;
    PrintWriter out;

    public void solve() throws IOException {
        int module = in.nextInt();
        int n = in.nextInt();
        int m = in.nextInt();
        Matrix[] array = new Matrix[n];
        for (int i = 0; i < n; i++) {
            array[i] = new Matrix(in.nextInt(), in.nextInt(), in.nextInt(), in.nextInt());
        }
        checkStamp("Readed all the stuff");
        SegmentTree st = new SegmentTree(array, 1, 0, n - 1, module);
        checkStamp("Builded the tree");
        for (int i = 0; i < m; i++) {
            int l = in.nextInt();
            int k = in.nextInt();
            out.println(st.query(1, 0, n - 1, l - 1, k - 1));
            out.println();
        }
        checkStamp("Performed operations");
    }

    public void run() {
        try {
            in = new FastScanner(new File("crypto.in"));
            out = new PrintWriter(new File("crypto.out"));

            solve();

            out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }
    }

    public static void main(String[] arg) {
        new C_crypto2().run();
    }

    class SegmentTree {
        int n, r;
        Matrix[] data;

        SegmentTree(Matrix[] a, int v, int left, int right, int r) {
            n = a.length;
            this.r = r;
            data = new Matrix[4 * n];
            build(a, v, left, right);
        }

        public void build(Matrix[] a, int v, int left, int right) {
            if (left == right)
                data[v] = a[left];
            else {
                int middle = (left + right) / 2;
                build(a, v * 2, left, middle);
                build(a, v * 2 + 1, middle + 1, right);
                data[v] = multiply(data[v * 2], data[v * 2 + 1]);
            }
        }

        public Matrix multiply(Matrix b, Matrix c) {
            return new Matrix((b.A * c.A + b.B * c.C) % r,
                    (b.A * c.B + b.B * c.D) % r,
                    (b.C * c.A + b.D * c.C) % r,
                    (b.C * c.B + b.D * c.D) % r);
        }

        public Matrix query(int v, int left, int right, int i, int j) {
            if (i > j)
                return Matrix.NEUTRAL;
            if (left == i && right == j)
                return data[v];
            int middle = (left + right) / 2;
            return multiply(query(v * 2, left, middle, i, min(j, middle)),
                    query(v * 2 + 1, middle + 1, right, max(i, middle + 1), j));
        }
    }
}

class Matrix {
    public static final Matrix NEUTRAL = new Matrix(1, 0, 0, 1);
    public int A, B, C, D;

    Matrix(int a, int b, int c, int d) {
        A = a;
        B = b;
        C = c;
        D = d;
    }

    @Override
    public String toString() {
        return A + " " + B + "\n" + C + " " + D;
    }
}