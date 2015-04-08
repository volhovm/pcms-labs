import labs.stringops.G_Search4;

import java.io.*;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Random;
import java.util.StringTokenizer;

/**
 * Created by volhovm on 3/23/14.
 */
public class test {
    private static final Random rand = new Random();

    public static void main(String[] args) throws IOException {
        PrintWriter scout = new PrintWriter(new FileWriter("count.in"));
        for (int i = 0; i < 100; i++) {
            StringBuilder str = new StringBuilder();
            for (int j = 0; j < rand.nextInt(40) + 10; j++) {
                str.append(((char) (rand.nextInt(15) + 'a')));
            }
            scout.println(str.toString());
        }
        for (int i = 0; i < 100; i++) {
            StringBuilder str = new StringBuilder();
            for (int j = 0; j < rand.nextInt(399990) + 10; j++) {
                str.append(((char) (rand.nextInt(26) + 'a')));
            }
            scout.println(str.toString());
        }
        scout.close();
    }

    private enum KeyWords {
        put(2), delete(2), get(1),
        deleteall(1)
//        , prev(1), next(1)
        ;

        KeyWords(int i) {
            argsNumber = i;
        }

        int argsNumber;
    }

    private static String randomPhrase(int argsLength) {
        String out = "";
        KeyWords[] wordArray = KeyWords.values();
        KeyWords chosenWord = (wordArray[rand.nextInt(100000) % wordArray.length]);
        out += chosenWord.toString();
        for (int i = 0; i < chosenWord.argsNumber; i++) {
            out += " " + randomMaybeBufferedString(argsLength);
        }
        return out;
    }

    private static String randomCharSequence(int length) {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < length; i++) {
            out.append(((char) (rand.nextInt('z' - 'a') + 'a')));
        }
        return out.toString();
    }

    private static ArrayList<String> buffer = new ArrayList<>();

    private static String randomMaybeBufferedString(int length) {
        if (!buffer.isEmpty() && rand.nextInt(4) != 3) {
            return buffer.get(rand.nextInt(buffer.size()));
        } else {
            String s = randomCharSequence(length);
            buffer.add(s);
            return s;
        }
    }

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
