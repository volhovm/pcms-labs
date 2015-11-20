package labs.sortsearch;

/**
 * @author volhovm
 * Created on 5/25/14
 */

import java.io.*;
import java.util.StringTokenizer;

public class H_netBuild {
    public static void main(String[] args) throws IOException {
        scin = new FastScanner(new File("netbuild.in"));
        scout = new PrintWriter(new File("netbuild.out"));
//        scout = new PrintWriter(new File("netcheck.in"));
        int n = scin.nextInt();
        scout.print(NetWorks.values()[n - 1].getString());
        scout.close();
    }

    public enum NetWorks{
        one("1 0 0"),
        two("2 1 1\r\n1 1 2"),
        three("3 3 3\r\n" +
                "1 2 3\r\n" +
                "1 1 3\r\n" +
                "1 1 2"),
        four("4 5 3\r\n" +
                "2 1 2 3 4\r\n" +
                "2 1 3 2 4\r\n" +
                "1 2 3"),
        fifth("5 9 6\r\n " +
                "2 1 2 4 5\r\n" +
                "1 3 5\r\n" +
                "2 3 4 2 5\r\n" +
                "1 1 4\r\n" +
                "2 1 3 2 4\r\n" +
                "1 2 3"),
        six("6 12 6\r\n" +
                "2 2 3 5 6\r\n" +
                "2 1 3 4 6\r\n" +
                "3 1 2 4 5 3 6\r\n" +
                "2 1 4 2 5\r\n" +
                "2 3 5 2 4\r\n" +
                "1 3 4"),
        seven("7 16 7\r\n" +
                "3 2 3 4 5 6 7\r\n" +
                "3 1 3 4 6 5 7\r\n" +
                "3 1 2 5 6 3 7\r\n" +
                "2 1 5 2 6\r\n" +
                "2 1 4 3 6\r\n" +
                "2 2 4 3 5\r\n" +
                "1 3 4\r\n"),
        eight("8 19 7\r\n" +
                "4 1 2 3 4 5 6 7 8 \r\n" +
                "4 1 3 2 4 5 7 6 8 \r\n" +
                "4 2 3 6 7 1 5 4 8\r\n" +
                "2 2 6 3 7\r\n" +
                "2 2 5 4 7\r\n" +
                "2 3 5 4 6\r\n" +
                "1 4 5"),
        nine("9 25 9\n" +
                "3 1 2 4 5 7 8 \n" +
                "3 2 3 5 6 8 9\n" +
                "4 1 2 4 5 7 8 3 6\n" +
                "3 1 4 2 5 6 9\n" +
                "3 4 7 5 8 3 6\n" +
                "4 1 4 2 5 6 8 3 7\n" +
                "2 2 4 5 7\n" +
                "2 3 5 6 7\n" +
                "1 3 4"),
        ten("10 29 9\r\n" +
                "5 5 10 4 9 3 8 2 7 1 6\r\n" +
                "4 2 5 7 10 1 4 6 9\r\n" +
                "3 1 3 4 7 8 10\r\n" +
                "4 1 2 3 5 6 8 9 10\r\n" +
                "4 2 3 5 7 8 9 4 6\r\n" +
                "4 3 6 7 9 2 4 5 8\r\n" +
                "2 3 4 7 8\r\n" +
                "2 4 5 6 7\r\n" +
                "1 5 6"),
        eleven("11 35 9\r\n" +
                "5 1 2 3 4 5 6 7 8 9 10\r\n" +
                "5 2 4 6 8 1 3 5 7 9 11\r\n" +
                "5 2 3 6 7 10 11 1 5 4 8\r\n" +
                "3 2 6 7 11 5 9\r\n" +
                "4 6 10 3 7 1 5 4 9\r\n" +
                "4 2 6 7 11 3 4 9 10\r\n" +
                "4 2 5 8 11 4 6 7 9\r\n" +
                "3 3 5 8 10 6 7\r\n" +
                "2 4 5 8 9"),
        twelve("12 39 9\r\n" +
                "6 1 2 3 4 5 6 7 8 9 10 11 12 \r\n" +
                "6 2 4 6 8 10 12 1 3 5 7 9 11 \r\n" +
                "5 2 3 6 7 10 11 1 5 8 12 \r\n" +
                "4 2 6 7 11 4 8 5 9 \r\n" +
                "5 6 10 3 7 1 5 8 12 4 9 \r\n" +
                "4 2 6 7 11 3 4 9 10 \r\n" +
                "4 2 5 8 11 4 6 7 9 \r\n" +
                "3 3 5 8 10 6 7 \r\n" +
                "2 4 5 8 9"),
        thirteen("13 45 10\r\n" +
                "6 2 8 10 12 4 5 6 9 1 13 3 7 \r\n" +
                "6 1 2 3 4 5 7 9 12 8 13 6 10 \r\n" +
                "5 1 3 4 8 11 12 2 5 7 13 \r\n" +
                "4 8 9 12 13 5 10 7 11 \r\n" +
                "5 4 5 6 7 9 10 11 12 2 8 \r\n" +
                "6 3 7 10 12 2 4 5 8 9 11 1 6 \r\n" +
                "3 3 6 7 9 10 11 \r\n" +
                "4 2 3 4 6 8 9 5 7 \r\n" +
                "4 3 4 5 6 7 8 9 10 \r\n" +
                "2 4 5 6 7"),
        fourteen("14 51 10\r\n" +
                "7 1 2 3 4 5 6 7 8 9 10 11 12 13 14 \r\n" +
                "6 1 3 5 7 9 11 2 4 6 8 10 12 \r\n" +
                "6 1 5 9 13 2 6 10 14 3 7 4 8 \r\n" +
                "6 1 9 2 10 3 11 4 12 5 13 6 14 \r\n" +
                "6 6 11 7 10 4 13 8 12 2 3 5 9 \r\n" +
                "5 2 5 8 14 3 9 6 7 10 11 \r\n" +
                "4 3 5 12 14 4 9 8 13 \r\n" +
                "4 7 9 11 13 4 6 8 10 \r\n" +
                "5 4 5 6 7 8 9 10 11 12 13 \r\n" +
                "2 7 8 9 10"),
        fifteen("15 56 10\r\n" +
                "7 1 2 3 4 5 6 7 8 9 10 11 12 13 14 \r\n" +
                "7 1 3 5 7 9 11 13 15 2 4 6 8 10 12 \r\n" +
                "7 1 5 9 13 2 6 10 14 3 7 11 15 4 8 \r\n" +
                "7 1 9 2 10 3 11 4 12 5 13 6 14 7 15 \r\n" +
                "7 6 11 7 10 4 13 14 15 8 12 2 3 5 9 \r\n" +
                "6 2 5 8 14 3 9 12 15 6 7 10 11 \r\n" +
                "4 3 5 12 14 4 9 8 13 \r\n" +
                "4 7 9 11 13 4 6 8 10 \r\n" +
                "5 4 5 6 7 8 9 10 11 12 13 \r\n" +
                "2 7 8 9 10 "),
        sixteen("16 60 10\r\n" +
                "8 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 \r\n" +
                "8 1 3 5 7 9 11 13 15 2 4 6 8 10 12 14 16 \r\n" +
                "8 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16 \r\n" +
                "8 1 9 2 10 3 11 4 12 5 13 6 14 7 15 8 16 \r\n" +
                "7 6 11 7 10 4 13 14 15 8 12 2 3 5 9 \r\n" +
                "6 2 5 8 14 3 9 12 15 6 7 10 11 \r\n" +
                "4 3 5 12 14 4 9 8 13 \r\n" +
                "4 7 9 11 13 4 6 8 10 \r\n" +
                "5 4 5 6 7 8 9 10 11 12 13 \r\n" +
                "2 7 8 9 10");

        private String a;
        NetWorks(String a) {
            this.a = a;
        }
        public String getString(){ return a; }
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
