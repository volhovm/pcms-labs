package labs.sortsearch;

/**
 * @author volhovm
 * Created on 5/25/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class G_netCheck {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("netcheck.in"));
        scout = new PrintWriter(new File("netcheck.out"));
        int n = scin.nextInt();
        int m = scin.nextInt();
        int k = scin.nextInt();
        SortingNetwork network = new SortingNetwork(n, k);
        for (int i = 0; i < k; i++){
            int curr = scin.nextInt();
            int[] currArr = new int[curr * 2];
            for (int j = 0; j < curr * 2; j++){
                currArr[j] = scin.nextInt() - 1;
            }
            network.addLayer(i, currArr);
        }
        scout.println(network.ifComparing() ? "YES" : "NO");
        scout.close();
    }

    private static class SortingNetwork {
        private class Comparator{
            int up, down;

            private Comparator(int a, int b) {
                this.up = a;
                this.down = b;
            }

            public void normalize(){
                if (up > down){
                    int temp = up;
                    up = down;
                    down = temp;
                }
            }
        }
        int wires;
        Comparator[][] comparators;

        public SortingNetwork(int wires, int layers) {
            this.wires = wires;
            this.comparators = new Comparator[layers][];
        }

        public void addLayer(int layer, int[] compPairs){
            if (comparators[layer] == null) {
                comparators[layer] = new Comparator[compPairs.length / 2];
            }
            for (int i = 0; i < compPairs.length / 2; i++){
                comparators[layer][i] = new Comparator(compPairs[i*2], compPairs[i*2 + 1]);
                comparators[layer][i].normalize();
            }
        }

        public boolean ifComparing(){
            boolean[] perm = new boolean[wires];
            int power = 1 << wires;
            for (int i = 0; i < power; i++){
                boolean[] permC = perm.clone();
                for (int j = 0; j < comparators.length; j++) { //forall layers
                    for (int k = 0; k < comparators[j].length; k++) { //forall comparators
                        int down = comparators[j][k].down;
                        int up = comparators[j][k].up;
                        boolean cu = min(permC[down], permC[up]);
                        boolean cd = max(permC[down], permC[up]);
                        permC[up] = cu;
                        permC[down] = cd;
                    }
                }
                if (!checkIfSorted(permC)) {
                    System.out.print("FAILED ON ");
                    for (int j = 0; j < perm.length; j++) {
                        if (!perm[j]) System.out.print("0 ");
                        else System.out.print("1 ");
                    }
                    System.out.println();
                    System.out.print("GOT ");
                    for (int j = 0; j < perm.length; j++) {
                        if (!permC[j]) System.out.print("0 ");
                        else System.out.print("1 ");
                    }
                    return false;
                }
                perm = nextPermutation(perm);
            }
            return true;
        }

        private boolean checkIfSorted(boolean[] permC) {
            boolean reachedTrue = false;
            for (boolean item : permC) {
                if (item) {
                    if (!reachedTrue) {
                        reachedTrue = true;
                    }
                } else {
                    if (reachedTrue) return false;
                }
            }
            return true;
        }

        private boolean max(boolean a, boolean b){
            return a != b || a;
        }

        private boolean min(boolean a, boolean b){
            return a == b && a;
        }

        private boolean[] nextPermutation(boolean[] perm){
            int changedInt = perm.length - 1;
            for (int i = perm.length - 1; i >=0 ; i--){
                if (!perm[i]){
                    changedInt = i;
                    perm[i] = true;
                    break;
                }
            }
            for (int i = changedInt + 1; i < perm.length; i++){
                perm[i] = false;
            }
            return perm;
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
