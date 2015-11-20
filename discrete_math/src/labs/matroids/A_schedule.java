package labs.matroids;

import javafx.util.Pair;

import java.io.*;
import java.util.*;

/**
 * @author volhovm
 *         Created on 5/4/15
 */
public class A_schedule {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("schedule.in"));
        scout = new PrintWriter(new File("schedule.out"));
        int n = scin.nextInt();
        List<Pair<Integer, Integer>> values = new LinkedList<>();
        int maxDelay = 0;
        for (int i = 0; i < n; i++) {
            int delay = scin.nextInt();
            maxDelay = Math.max(maxDelay, delay);
            values.add(new Pair<>(delay, scin.nextInt()));
        }
        Collections.sort(values, (a, b) -> {
            int init = b.getValue().compareTo(a.getValue());
            if (init != 0) return init;
            else return a.getKey().compareTo(b.getKey());
        });
        long overallWeight = 0;
        class IntWrapper {
            int val;

            public IntWrapper(int i) {
                if (i < -1) throw new IllegalArgumentException();
                this.val = i;
            }

            @Override
            public String toString() {
                return String.valueOf(val);
            }
        }
        IntWrapper[] links = new IntWrapper[maxDelay + 1];
        for (int i = 0; i < links.length; i++) {
            links[i] = new IntWrapper(i - 1);
        }
        for (Pair<Integer, Integer> p : values) {
            IntWrapper curr = links[p.getKey()];
            int i = curr.val;
            if (i < 0) {
                overallWeight += p.getValue();
                continue;
            }
            if (links[i].val == i - 1) {
                links[i] = curr;
                if (curr.val != -1) curr.val--;
            } else {
                for (int j = i + 1; j < links.length; j++) {
                    if (links[j] == curr) {
                        links[j] = links[i];
                    } else break;
                }
            }
        }
        scout.println(overallWeight);
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
