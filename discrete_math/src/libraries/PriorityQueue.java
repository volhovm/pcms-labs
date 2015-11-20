package libraries;

import java.util.ArrayList;

/**
 * Created by volhovm on 3/21/14.
 */

//-------------------------
//   NOT SURE IF WORKS
// BUT WORKED FOR SORTING!
//-------------------------

public class PriorityQueue {
    ArrayList<Integer> mainArray = new ArrayList<>();
    int heapSize = 0;

    public void add(int value) {
        mainArray.add(heapSize, value);
        siftUp(heapSize++);
    }

    public int extractMin() {
        if (this.isEmpty()) {
            throw new IndexOutOfBoundsException();
        } else {
            int heapRoot = mainArray.get(0);
            mainArray.set(0, mainArray.get(heapSize - 1));
            heapSize--;
            siftDown(0);
            return heapRoot;
        }
    }

    public void changeKey(int index, int value) {
        int currentValue = mainArray.get(index);
        mainArray.set(index, value);
        if (currentValue > value) {
            siftUp(index);
        } else if (currentValue < value) {
            siftDown(index);
        }
    }

    private void siftUp(int index) {
        if (index > 0) {
            int current = mainArray.get(index);
            int parentOfCurrent = mainArray.get((index - 1) / 2);
            if (current < parentOfCurrent) {
                mainArray.set(index, parentOfCurrent);
                mainArray.set((index - 1) / 2, current);
                siftUp((index - 1) / 2);
            }
        }
    }

    private void siftDown(int index) {
        if (index * 2 + 1 < heapSize) {
            int current = mainArray.get(index);
            int leftChild = mainArray.get(index * 2 + 1);
            if (index * 2 + 2 < heapSize) { //if this has right child too
                int rightChild = mainArray.get(index * 2 + 2);
                if (leftChild < current || rightChild < current) {
                    int minChild;
                    int minChildIndex;
                    if (leftChild < rightChild) {
                        minChild = leftChild;
                        minChildIndex = index * 2 + 1;
                    } else {
                        minChild = rightChild;
                        minChildIndex = index * 2 + 2;
                    }
                    //swapping
                    mainArray.set(index, minChild);
                    mainArray.set(minChildIndex, current);
                    siftDown(minChildIndex);
                }
            } else {
                if (leftChild < current) {
                    mainArray.set(index, leftChild);
                    mainArray.set(index * 2 + 1, current);
                }
            }
        }
    }

    public int size() {
        return heapSize;
    }

    public boolean isEmpty() {
        return heapSize == 0;
    }
}