package labs.harderstructures;
/**
 *   Created by volhovm on 3/22/14
 */

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

public class C_priorityQueue {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("priorityqueue.in"));
        scout = new PrintWriter(new File("priorityqueue.out"));
        PriorityQueue queue = new PriorityQueue();
        try {
            for (int i = 1; true; i++) {
                String current = scin.next();
                if (current.equals("push")) {
                    queue.add(i, scin.nextInt());
                } else if (current.equals("extract-min")) {
                    try {
                        scout.println(queue.extractMin());
                    } catch (IndexOutOfBoundsException ignored) {
                        scout.println("*");
                    }
                } else if (current.startsWith("decrease-key")) {
                    //13 ?? 42?? Stsastvdavgdvadhfj mom pizu))
                    int a = scin.nextInt();
                    int b = scin.nextInt();
                    queue.changeKey(a, b);
                } else {
                    throw new IllegalArgumentException("Strange command");
                }
            }
        } catch (NullPointerException ignored) {
        }
        scout.close();
    }


    private static class PriorityQueue {

        private class YobaPair {
            int pushIndex;
            int value;

            YobaPair(int pushIndex, int value) {
                this.pushIndex = pushIndex;
                this.value = value;
            }
        }

        int heapSize;
        ArrayList<YobaPair> mainArray;
        int[] addressList;

        public PriorityQueue() {
            mainArray = new ArrayList<>();
            addressList = new int[10_000_000];
            Arrays.fill(addressList, -1);
            heapSize = 0;
        }

        public void add(int pushIndex, int value) {
            mainArray.add(heapSize, new YobaPair(pushIndex, value));
            addressList[pushIndex] = heapSize;
            siftUp(heapSize++);
        }

        public int extractMin() {
            if (this.isEmpty()) {
                throw new IndexOutOfBoundsException();
            } else {
                int heapRoot = mainArray.get(0).value;
                addressList[mainArray.get(0).pushIndex] = -1;
                addressList[mainArray.get(heapSize - 1).pushIndex] = 0;
                mainArray.get(0).value = mainArray.get(heapSize - 1).value;
                mainArray.get(0).pushIndex = mainArray.get(heapSize - 1).pushIndex;
                heapSize--;
                siftDown(0);
                return heapRoot;
            }
        }

        public void changeKey(int commandNumber, int value) {
            int index = addressList[commandNumber];
            if (index != -1) {
                int currentValue = mainArray.get(index).value;
                mainArray.get(index).value = value;
                if (currentValue > value) {
                    siftUp(index);
                } else if (currentValue < value) {
                    siftDown(index);
                }
            }
        }

        private void siftUp(int index) {
            if (index > 0) {
                int currentValue = mainArray.get(index).value;
                int parentOfCurrentValue = mainArray.get((index - 1) / 2).value;
                if (currentValue < parentOfCurrentValue) {
                    //swapping
                    int tempPushIndex = mainArray.get(index).pushIndex;
                    addressList[mainArray.get(index).pushIndex] = (index - 1) / 2;
                    addressList[mainArray.get((index - 1) / 2).pushIndex] = index;
                    mainArray.get(index).value = parentOfCurrentValue;
                    mainArray.get(index).pushIndex = mainArray.get((index - 1) / 2).pushIndex;
                    mainArray.get((index - 1) / 2).value = currentValue;
                    mainArray.get((index - 1) / 2).pushIndex = tempPushIndex;

                    siftUp((index - 1) / 2);
                }
            }
        }

        private void siftDown(int index) {
            if (index * 2 + 1 < heapSize) { // if this has left child
                int current = mainArray.get(index).value;
                int leftChild = mainArray.get(index * 2 + 1).value;
                if (index * 2 + 2 < heapSize) { //if this has right child too
                    int rightChild = mainArray.get(index * 2 + 2).value;
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
                        int temp = mainArray.get(index).pushIndex;
                        addressList[mainArray.get(index).pushIndex] = minChildIndex;
                        addressList[mainArray.get(minChildIndex).pushIndex] = index;
                        mainArray.get(index).value = minChild;
                        mainArray.get(index).pushIndex = mainArray.get(minChildIndex).pushIndex;
                        mainArray.get(minChildIndex).value = current;
                        mainArray.get(minChildIndex).pushIndex = temp;

                        siftDown(minChildIndex);
                    }
                } else {
                    if (leftChild < current) {
                        int temp = mainArray.get(index).pushIndex;
                        addressList[mainArray.get(index).pushIndex] = index * 2 + 1;
                        addressList[mainArray.get(index * 2 + 1).pushIndex] = index;
                        mainArray.get(index).value = leftChild;
                        mainArray.get(index).pushIndex = mainArray.get(index * 2 + 1).pushIndex;
                        mainArray.get(index * 2 + 1).value = current;
                        mainArray.get(index * 2 + 1).pushIndex = temp;
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

    public static FastScanner scin;
    public static PrintWriter scout;

    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        double nextDouble() {
            return Double.parseDouble(next());
        }
    }
}
