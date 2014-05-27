package libraries;

import java.util.LinkedList;

/**
 * @author volhovm
 *         Created on 18.04.14
 */


public class SimpleHashMap<K, V> {
    private final static int HASHMAPCAPACITY = 100_000;
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
