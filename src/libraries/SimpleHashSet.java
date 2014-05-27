package libraries;

import java.util.LinkedList;

/**
 * @author volhovm
 *         Created on 17.04.14
 */


public class SimpleHashSet {
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
        if (!exists(value)) {
            int k = hash(value);
            if (table[k] == null) {
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
