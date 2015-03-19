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
        PrintWriter scout = new PrintWriter(new FileWriter("search4.in"));
        ArrayList<String> patterns = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            String s = "";
            for (int j = 0; j < rand.nextInt(15) + 1; j++) {
                s += (char) ('a' + rand.nextInt(10));
            }
            patterns.add(s);
        }
        String text = "";
        for (int i = 0; i < 10000; i++) {
            text += (char) ('a' + rand.nextInt(10));
        }
        scout.println(patterns.size());
        patterns.stream().forEach(scout::println);
        scout.print(text);
        scout.flush();
        scout.close();
        G_Search4.main(null);
        FastScanner scin = new FastScanner(new File("search4.out"));
        ArrayList<Boolean> ans = new ArrayList<>();
        for (int i = 0; i < patterns.size(); i++) {
            String next = scin.next();
            ans.add(Objects.equals(next, "YES"));
        }
        for (int i = 0; i < patterns.size(); i++) {
            String pattern = patterns.get(i);
            for (int j = 0; j < text.length() - pattern.length(); j++) {
                for (int k = 0; k < pattern.length(); k++) {
                    if (text.charAt(j + k) != pattern.charAt(k)) break;
                    if (k == pattern.length() - 1)
                        if (!ans.get(i)) {
                            System.out.println(i);
                        }
                }
            }
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
