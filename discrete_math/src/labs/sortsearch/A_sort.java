package labs.sortsearch;/**
 * @author volhovm
 * Created on 5/21/14
 */

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class A_sort {
    private static long timeStampStart = System.currentTimeMillis();
    private static long tempStamp = timeStampStart;
    private static void checkTime(){
        System.out.println(System.currentTimeMillis() - tempStamp);
        tempStamp = System.currentTimeMillis();
    }

    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("sort.in"));
        scout = new PrintWriter(new File("sort.out"));
        int n = scin.nextInt();
        PriorityQueue queue = new PriorityQueue();
        for (int i = 0; i < n; i++){
            queue.add(scin.nextInt());
        }
//        arr = heapSort(arr);
//        Arrays.sort(secarr);
        for (int i = 0; i < n; i++) {
            scout.print(queue.extractMin() + " ");
        }
//        for (int i = 0; i < n; i++) {
//            if (secarr[i] != arr[i]) System.out.println(secarr[i] + " != " + arr[i]);
//        }
//        System.out.println(System.currentTimeMillis() - timeStampStart);
        scout.close();
    }

    private static int[] heapSort(int[] arr){
        Heap heap = new Heap(arr);
        checkTime();
        heap.sortIt();
        return heap.getArray();
    }

    static public class PriorityQueue {
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

    private static class Heap{
        private int[] arr;
        private int size;

        public Heap(int[] array){
            this.size = array.length;
            arr = new int[size];
            arr = array;
            siftDown(0);
        }

        public void sortIt(){
            for (int i = size; i > 0; i--){
                swap(0, size - 1);
                size--;
                checkTime();
                siftDown(0);
            }
        }

        private void siftDown(int i) {
            if (i >= size) return;
            if (i * 2 + 2 >= size){
                if (i * 2 + 1 >= size){
                    return;
                }
                if (arr[i] < arr[i * 2 + 1]){
                    swap(i, i * 2 + 1);
                }
            } else {
                if (arr[i] < arr[i * 2 + 1] || arr[i] < arr[i * 2 + 2]) {
                    if (arr[i * 2 + 2] > arr[i * 2 + 1]) {
                        swap(i, i * 2 + 2);
                    } else {
                        swap(i, i * 2 + 1);
                    }
                }
                siftDown(i * 2 + 1);
                siftDown(i * 2 + 2);
            }
        }

        private void swap(int i, int j){
            int temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
        }

        public int[] getArray(){
            return arr;
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
