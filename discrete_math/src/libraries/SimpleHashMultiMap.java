package libraries;


import java.util.LinkedList;

/**
 * @author volhovm
 *         Created on 18.04.14
 */


public class SimpleHashMultiMap<K, V> {
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

    public LinkedList<V> get(K key){
        return table.get(key);
    }

    public void delete(K key, V value){
        LinkedList<V> curr = table.get(key);
        int i = curr.indexOf(value);
        if (i != -1) curr.remove(i);
    }

    public void deleteAll(K key){
        table.get(key).clear();
    }
}
