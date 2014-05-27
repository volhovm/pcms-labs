package labs.hashtables;
/**
 * @author volhovm
 * Created on 18.04.14
 */


import java.io.*;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class A_set {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("set.in"));
        scout = new PrintWriter(new File("set.out"));
        SimpleHashSet set = new SimpleHashSet();
        try {
            while (true) {
                String s = scin.next();
                switch (s) {
                    case "insert":
                        set.insert(scin.nextInt());
                        break;
                    case "exists":
                        scout.println(set.exists(scin.nextInt()));
                        break;
                    case "delete":
                        set.delete(scin.nextInt());
                        break;
                }
            }
        } catch (IOException exc){}
        scout.close();
    }

    static class SimpleHashSet {
        private final static int HASHSETCAPACITY = 100000;
        LinkedList<Integer>[] table;
        public SimpleHashSet(){
            table = new LinkedList[HASHSETCAPACITY];
            //        Arrays.fill(table, new LinkedList<Integer>());
        }

        public boolean exists(int value){
            int k = hash(value);
            if (table[k] == null) return false;
            for (int i = 0; i < table[k].size(); i++) {
                if (table[k].get(i) == value) return true;
            }
            return false;
        }

        public void insert(int value){
            if (!exists(value)){
                int k = hash(value);
                if (table[k] == null){
                    table[k] = new LinkedList<Integer>();
                }
                table[k].addLast(value);
            }
        }

        public void delete(int value){
            int k = hash(value);
            if (table[k] != null){
                for (int i = 0; i < table[k].size(); i++) {
                    if (table[k].get(i) == value) {
                        table[k].remove(i);
                        break;
                    }
                }
            }
        }

        private int hash(int key){
            return Math.abs(key) % HASHSETCAPACITY;
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
