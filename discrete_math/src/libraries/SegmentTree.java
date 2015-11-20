package libraries;

import java.util.Arrays;
import java.util.function.BiFunction;

/**
 * @author volhovm
 *         Created on 02.05.14
 */
//@SuppressWarnings("unchecked")
public class SegmentTree<T> {
    private T[] mainArray;
    private final T neutral;
    private final BiFunction<T, T, T> operation;
    private int capacity;

    public SegmentTree(int size, T neutralElement, BiFunction<T, T, T> binaryOperation) {
        //noinspection StatementWithEmptyBody
        for (capacity = 1; capacity < size; capacity *= 2) {
        }
        //noinspection unchecked
        mainArray = (T[]) new Object[2 * capacity];
        Arrays.fill(mainArray, neutralElement);
        neutral = neutralElement;
        operation = binaryOperation;
    }

    public void set(int index, T value) {
        mainArray[capacity + index] = value;
        int v = capacity + index;
        v = v / 2;
        while (v > 0) {
            mainArray[v] = operation.apply(mainArray[2 * v], mainArray[2 * v + 1]);
            v = v / 2;
        }
    }

    private T get(int currL, int currR, int l, int r, int node){
        if (l <= currL && currR <= r) {
            return mainArray[node];
        }
        if (r <= currL || l >= currR){
            return neutral;
        }
        int mid = (currL + currR) / 2;
        return operation.apply(get(currL, mid, l, r, node * 2), get(mid, currR, l, r, node * 2 + 1));
    }

    public T get(int left, int right){
        return get(0, capacity, left, right, 1);
    }
}

