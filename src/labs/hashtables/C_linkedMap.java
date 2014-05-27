package labs.hashtables;
/**
 * @author volhovm
 * Created on 18.04.14
 */


import java.io.*;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class C_linkedMap {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("linkedmap.in"));
        scout = new PrintWriter(new File("linkedmap.out"));
        SimpleLinkedHashMap map = new SimpleLinkedHashMap();
        try{
            while (true){
                String s = scin.next();
                switch (s){
                    case "put":
                        map.put(scin.next(), scin.next());
                        break;
                    case "get":
                        scout.println(map.get(scin.next()));
                        break;
                    case "prev":
                        scout.println(map.prev(scin.next()));
                        break;
                    case "next":
                        scout.println(map.next(scin.next()));
                        break;
                    case "delete":
                        map.delete(scin.next());
                        break;
                }
            }
        } catch (IOException exc){
            System.err.println(exc.getMessage());
        }
        scout.close();
    }

    static class SimpleLinkedHashMap {
        private final static int HASHMAPCAPACITY = 100_000;
        LinkedList<KeyValuePair<String>>[] table;
        private KeyValuePair<String> tail;

        public SimpleLinkedHashMap() {
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

        public void put(String key, String value) {
            KeyValuePair<String> curr = getPair(key);
            if (curr == null) {
                int k = hash(key);
                if (table[k] == null) {
                    table[k] = new LinkedList<>();
                }
                table[k].addLast(new KeyValuePair<String>(key, value, null, null));
                if (tail != null) {
                    table[k].getLast().prev = tail;
                    tail.next = table[k].getLast();
                }
                tail = table[k].getLast();
            } else {
                curr.value = value;
            }
        }

        public void delete(String key) {
            KeyValuePair current = null;
            int k = hash(key);
            int index = -1;
            if (table[k] == null) {
                return;
            }
            for (int i = 0; i < table[k].size(); i++) {
                if (table[k].get(i).key.equals(key)) {
                    index = i;
                    current = table[k].get(i);
                    break;
                }
            }
            if (current == null) return;
            if (current == tail) {
                tail = tail.prev;
            } else {
                current.next.prev = current.prev;
                if (current.prev != null) {
                    current.prev.next = current.next;
                }
            }
            table[k].remove(index);
        }

        public String next(String key) {
            KeyValuePair<String> curr = getPair(key);
            return curr == null || curr == tail ? "none" : curr.next.value;
        }

        public String prev(String key) {
            KeyValuePair<String> curr = getPair(key);
            return curr == null || curr.prev == null ? "none" : curr.prev.value;
        }

        private int hash(String key) {
            return Math.abs(key.hashCode()) % HASHMAPCAPACITY;
        }

        private class KeyValuePair<T> {
            public T key, value;
            public KeyValuePair<T> prev, next;

            private KeyValuePair(T key, T value, KeyValuePair<T> prev,
                                 KeyValuePair<T> next) {
                this.key = key;
                this.value = value;
                this.prev = prev;
                this.next = next;
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
