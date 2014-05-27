package labs.sortsearch;

/**
 * @author volhovm
 * Created on 5/21/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class B_binSearch {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("binsearch.in"));
        scout = new PrintWriter(new File("binsearch.out"));
        int n = scin.nextInt();
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) arr[i] = scin.nextInt();
        int m = scin.nextInt();
        for (int i = 0; i < m; i++){
            int curr = scin.nextInt();
            int first = binsearch(arr, curr);
            if (first == -1 || first == arr.length || arr[first] != curr) {
                scout.println("-1 -1");
            } else {
                scout.println((first + 1) + " " + binsearch(arr, curr + 1));
            }
        }
        scout.close();
    }

    //return index
    private static int binsearch(int[] array, int value){
        if (array[0] > value) return -1;
        if (array[array.length - 1] < value) return array.length;
        return search(array, value, 0, array.length);
    }

    private static int search(int[] array, int value, int l, int r) {
        if (l + 1 == r){
            return array[l] < value ? l + 1 : l;
        }
        int median = (l + r) / 2;
        if (array[median] < value) return search(array, value, median, r);
        else return search(array, value, l, median);
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
