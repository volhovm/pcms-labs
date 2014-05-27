package labs.simplestructures;
//Jenya Vinogradov's code

import java.io.*;
import java.util.StringTokenizer;

public class Queuemin2 {
    FastScanner in;
    PrintWriter out;

    public static void main(String[] arg) {
        new Queuemin2().run();
    }

    public void solve() throws IOException {
        int n = in.nextInt();
        int m = in.nextInt();
        int k = in.nextInt();
        int a = in.nextInt();
        int b = in.nextInt();
        int c = in.nextInt();
        Queue q = new Queue();
        int cur, pr = 0, ppr = 0;
        long sum = 0;
        for (int i = 0; i < n; i++) {
            if (i < k) {
                cur = in.nextInt();
            } else {
                cur = a * ppr + b * pr + c;
            }
            q.put(cur);
            if (i >= m - 1) {
                sum += q.qMin();
                q.forget();
            }
            // prevs
            ppr = pr;
            pr = cur;
        }
        out.write(Long.toString(sum));
    }

    public void run() {
        try {
            in = new FastScanner(new File("queuemin2.in"));
            out = new PrintWriter(new File("queuemin2.out"));

            solve();

            out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    class FastScanner {
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
    }

    class Queue {
        Element head, tail;
        int size = 0;
        public void put(int n) {
            Element nev = new Element();
            nev.min = n;
            if (size == 0) {
                nev.count = 1;
                head = nev;
                tail = nev;
            } else {
                Element cur = tail;
                int count = 1;
                while (cur != null && cur.min >= n) {
                    count += cur.count;
                    cur = cur.prev;
                }
                nev.count = count;
                if (size == count) {
                    head.next = nev;
                    nev.prev = head;
                    tail = nev;
                } else if (count > size) {
                    head = nev;
                    tail = nev;
                } else {
                    cur.next = nev;
                    nev.prev = cur;
                    tail = nev;
                }
            }
            size++;
        }

        public int qMin() {
            //System.out.println("min " + Integer.toString(head.min));
            return head.min;
        }

        public void forget() {
            head.count--;
            size--;
            if (size == 0) {
                head = null;
                tail = null;
            } else if (head.count == 0) {
                head.next.prev = head.prev;
                head = head.next;
            }
        }
    }

    class Element {
        int min, count;
        Element next, prev;
    }
}
