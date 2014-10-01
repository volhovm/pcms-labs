import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Random;

/**
 * Created by volhovm on 3/23/14.
 */
public class test {
    private static final Random rand = new Random();
    public static void main(String[] args) throws IOException {
        PrintWriter scout = new PrintWriter(new FileWriter("sort.in"));
//        for (int i = 0; i < 100_000; i++) {
//            scout.println(randomPhrase(3));
//        }
        scout.print("100000\n");
        for (int i = 0; i < 100000; i++) scout.print(rand.nextInt() + " ");
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

    private static String randomCharSequence(int length){
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < length; i++) {
            out.append(((char) (rand.nextInt('z' - 'a') + 'a')));
        }
        return out.toString();
    }

    private static ArrayList<String> buffer = new ArrayList<>();
    private static String randomMaybeBufferedString(int length){
        if (!buffer.isEmpty() && rand.nextInt(4) != 3){
            return buffer.get(rand.nextInt(buffer.size()));
        } else {
            String s = randomCharSequence(length);
            buffer.add(s);
            return s;
        }
    }
}
