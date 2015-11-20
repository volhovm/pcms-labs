package labs.hashtables;
/**
 * @author volhovm
 * Created on 18.04.14
 */

import java.io.*;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class D_multiMap {
    public static void main(String[] args) throws IOException {
        double startPoint = System.currentTimeMillis();
        scin = new FastScanner(new File("multimap.in"));
        scout = new PrintWriter(new File("multimap.out"));
        SimpleHashMultiMap<String, String> multiMap = new SimpleHashMultiMap<>();
        try {
            while (true){
                String s = scin.next();
                switch (s){
                    case "put":
                        multiMap.put(scin.next(), scin.next());
                        break;
                    case "get":
                        scout.println(multiMap.get(scin.next()));
                        break;
                    case "delete":
                        multiMap.delete(scin.next(), scin.next());
                        break;
                    case "deleteall":
                        multiMap.deleteAll(scin.next());
                        break;
                }
            }
        } catch (IOException exc) {
            System.err.println(exc.getMessage());
        }
        scout.close();
        System.out.println(System.currentTimeMillis() - startPoint);
    }

    static class SimpleHashMultiMap<K, V> {
        private SimpleHashMap<K, LinkedList<V>> table;

        @SuppressWarnings("UnusedDeclaration")
        public SimpleHashMultiMap(){
            table = new SimpleHashMap<>();
        }

        public void put(K key, V value){
            if (!table.exists(key)) {
                table.put(key, new LinkedList<V>());
            }
            if (!table.get(key).contains(value)) {
                table.get(key).add(value);
            }
        }

        public String get(K key){
            LinkedList<V> curr = table.get(key);
            if (curr == null){
                return "0";
            }
            StringBuilder z = new StringBuilder();
            z.append(curr.size());
            for (V aCurr : curr) {
                z.append(" ").append(aCurr);
            }
            z.append("\r\n");
            return z.toString();
        }

        public void delete(K key, V value){
            LinkedList<V> curr = table.get(key);
            if (curr != null) {
                int i = curr.indexOf(value);
                if (i != -1)
                    curr.remove(i);
            }
        }

        public void deleteAll(K key){
            LinkedList a = table.get(key);
            if (a != null) a.clear();
        }
    }

    static class SimpleHashMap<K, V> {
        private final static int HASHMAPCAPACITY = 1_000_000;
        LinkedList<KeyValuePair<K, V>>[] table;

        public SimpleHashMap(){
            table = new LinkedList[HASHMAPCAPACITY];
        }

        public boolean exists(K key) {
            return getPair(key) != null;
        }

        private KeyValuePair<K, V> getPair(K key) {
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

        public V get(K key) {
            KeyValuePair<K, V> curr = getPair(key);
            return (curr == null ? null : curr.value);
        }

        public void put(K key, V value){
            KeyValuePair<K, V> s = getPair(key);
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

        public void delete(K key){
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

        private int hash(K key){
            return Math.abs(key.hashCode()) % HASHMAPCAPACITY;
        }

        private class KeyValuePair<A, B>{
            public A key;
            public B value;

            private KeyValuePair(A key, B value) {
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
