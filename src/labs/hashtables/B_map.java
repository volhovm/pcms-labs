package labs.hashtables;
/**
 * @author volhovm
 * Created on 18.04.14
 */



import java.io.*;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class B_map {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("map.in"));
        scout = new PrintWriter(new File("map.out"));
        SimpleHashMap map = new SimpleHashMap();
        try{
            while (true){
                String req = scin.next();
                switch (req){
                    case "put":
                        map.put(scin.next(), scin.next());
                        break;
                    case "get":
                        String out = map.get(scin.next());
                        scout.println(out == null ? "none" : out);
                        break;
                    case "delete":
                        map.delete(scin.next());
                        break;
                }
            }
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        scout.close();
    }

    static class SimpleHashMap {
        private final static int HASHMAPCAPACITY = 100_000;
        LinkedList<KeyValuePair<String>>[] table;
        public SimpleHashMap(){
            //noinspection unchecked
            table = new LinkedList[HASHMAPCAPACITY];
        }

        public boolean exists(String key) {
            return getPair(key) != null;
        }

        private KeyValuePair<String> getPair(String key) {
            int k = hash(key);
            if (table[k] == null) {
                return null;
            }
            for (int i = 0; i < table[k].size(); i++) {
                if (table[k].get(i).key.equals(key)) {
                    return table[k].get(i);
                }
            }
            return null;
        }

        public String get(String key) {
            KeyValuePair<String> curr = getPair(key);
            return curr == null ? "none" : curr.value;
        }

        public void put(String key, String value){
            KeyValuePair<String> s = getPair(key);
            if (s == null) {
                int k = hash(key);
                if (table[k] == null) {
                    table[k] = new LinkedList<>();
                }
                table[k].addLast(new KeyValuePair<>(key, value));
            } else {
                s.value = value;
            }
        }

        public void delete(String key){
            int k = hash(key);
            if (table[k] != null){
                for (int i = 0; i < table[k].size(); i++) {
                    if (table[k].get(i).key.equals(key)) {
                        table[k].remove(i);
                        break;
                    }
                }
            }
        }

        private int hash(String key){
            return Math.abs(key.hashCode()) % HASHMAPCAPACITY;
        }

        private class KeyValuePair<T>{
            public T key;
            public T value;

            private KeyValuePair(T key, T value) {
                this.key = key;
                this.value = value;
            }
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
