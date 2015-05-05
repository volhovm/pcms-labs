package labs.matroids;

import java.io.*;
import java.util.StringTokenizer;

/**
 * @author volhovm
 *         Created on 5/5/15
 */
public class E_cycles {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("cycles.in"));
        scout = new PrintWriter(new File("cycles.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();

        scout.close();
    }

        private static class BitSet {
        private final int set;
        private final int size;

        public BitSet(int set) {
            if (set < 0) throw new IllegalArgumentException();
            this.set = set;
            int size = 0;
            for (int i = 0; i < 10; i++) {
                if ((1 << i) == ((1 << i) & set)) size++;
            }
            this.size = size;
        }

        public static BitSet zero() {
            return new BitSet(0);
        }

//        public static BitSet singleton(int num) {
//            return new BitSet(1 << num);
//        }

        public BitSet sub(BitSet other) {
            return new BitSet(set ^ (set & other.set));
        }

        public BitSet unite(BitSet other) {
            return new BitSet(set | other.set);
        }

        public boolean isSubsetOf(BitSet other) {
            return set == (set & other.set);
        }

        public List<BitSet> subsets() {
            List<BitSet> candidates = new ArrayList<>();
//            for (int s = set; s != 0; s = (s - 1) & set) candidates.add(new BitSet(s));
            for (int i = 0; i < 1 << 10; i++) {
                if (i == (i & set)) candidates.add(new BitSet(i));
            }
            return candidates;
        }

        public ArrayList<BitSet> items() {
            ArrayList<BitSet> ret = new ArrayList<>();
            for (int i = 0; i < 10; i++) {
                if ((1 << i) == ((1 << i) & set)) {
                    ret.add(new BitSet(1 << i));
                }
            }
            return ret;
        }

        public int size() {
            return size;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            BitSet bitSet = (BitSet) o;
            return set == bitSet.set;
        }

        @Override
        public int hashCode() {
            return set;
        }

        @Override
        public String toString() {
            String out = "";
            for (int i = 0; i < 10; i++) {
                if ((1 << i) == ((1 << i) & set)) out += (i + 1) + " ";
            }
            return out.trim();
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
