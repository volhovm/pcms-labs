package labs.segmenttree;
/**
 * @author volhovm
 * Created on 02.05.14
 */

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.StringTokenizer;

public class C_crypto {
    private static double start = System.currentTimeMillis();
    private static void checkStamp(String msg){
        System.out.println(msg + " " + (System.currentTimeMillis() - start));
        start = System.currentTimeMillis();
    }

    public static void main(String[] args) throws IOException {
//        BufferedReader scn = new BufferedReader(new FileReader("crypto.in"));
//        System.out.println(scn.readLine());
        scin = new FastScanner(new File("crypto.in"));
        scout = new PrintWriter(new File("crypto.out"));
        module = scin.nextInt();
        int n = scin.nextInt();
        int m = scin.nextInt();
        SegmentTree segmentTree = new SegmentTree(n, Matrix.NEUTRAL);
        segmentTree.rangeSet(n);
//        Matrix[] initArr = new Matrix[n];
//        for (int i = 0; i < n; i++) {
//            initArr[i] = new Matrix(scin.nextInt(), scin.nextInt(), scin.nextInt(), scin.nextInt());
//        }
//        segmentTree.rangeSet(initArr);
        for (int i = 0; i < m; i++) {
            scout.println(segmentTree.get(scin.nextInt() - 1, scin.nextInt()));
        }
        scout.close();
        checkStamp("Proceeding operations: ");
    }

    private static int module;

    /*
        a b  i e
        c d  f g

        ai + bf     ae + bg
        ci + df     ce + dg
    */
    private static class Matrix{
        public static final Matrix NEUTRAL = new Matrix(1, 0, 0, 1);
        final int a, b;
        final int c, d;

        private Matrix(int a, int b, int c, int d) {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
        }

        public Matrix mul(Matrix x){
            return new Matrix(
                    (a * x.a + b * x.c) % module,
                    (a * x.b + b * x.d) % module,
                    (c * x.a + d * x.c) % module,
                    (c * x.b + d * x.d) % module
            );
        }

        public String toString(){
            return a + " " + b + "\r\n" + c + " " + d + "\r\n";
        }
    }

    public static class SegmentTree {
        private Matrix[] mainArray;
        private final Matrix neutral;
        private int capacity;

        public SegmentTree(int size, Matrix neutralElement) {
            //noinspection StatementWithEmptyBody
            for (capacity = 1; capacity < size; capacity *= 2) {
            }
            mainArray = new Matrix[2 * capacity];
            Arrays.fill(mainArray, neutralElement);
            neutral = neutralElement;
        }

        public void rangeSet(int n) throws IOException {
            checkStamp("Initializing: ");
            for (int i = 0; i < n; i++) {
                mainArray[capacity+i] = new Matrix(scin.nextInt(), scin.nextInt(), scin.nextInt(), scin.nextInt());
            }
            checkStamp("Reading and filling: ");
            int i = capacity/2;
            while (i != 0){
                for (int j = i; j < i * 2; j++) {
                    mainArray[j] = mainArray[j*2].mul(mainArray[j*2+1]);
                }
                i = i / 2;
            }
            checkStamp("Creating top tree: ");
        }

        private Matrix get(int currL, int currR, int l, int r, int node){
            if (l <= currL && currR <= r) {
                return mainArray[node];
            }
            if (r <= currL || l >= currR){
                return neutral;
            }
            int mid = (currL + currR) / 2;
            return get(currL, mid, l, r, node * 2).mul(get(mid, currR, l, r, node * 2 + 1));
        }

        public Matrix get(int left, int right){
            return get(0, capacity, left, right, 1);
        }
    }

    public static FastScanner scin;
    public static PrintWriter scout;

    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) throws IOException {
            try {
                br = new BufferedReader(new InputStreamReader(new FileInputStream(f), Charset.forName("UTF-8")));
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

    private static class TheBetterScanner{
        private RandomAccessFile aFile;
        private FileChannel inChannel;
        private ByteBuffer buffer;

        public TheBetterScanner(String s) throws FileNotFoundException {
            aFile = new RandomAccessFile
                    (s, "r");
            inChannel = aFile.getChannel();
            buffer = ByteBuffer.allocate(1024);
        }
        public void test() throws IOException {
            while(inChannel.read(buffer) > 0)
            {
                buffer.flip();
                for (int i = 0; i < buffer.limit(); i++)
                {
                    System.out.print((char) buffer.get());
                }
                buffer.clear(); // do something with the data and clear/compact it.
            }
            inChannel.close();
            aFile.close();
        }
    }
}
