package libraries;

/**
 * @author volhovm
 *         Created on 3/16/15
 */

public class Strings {
    private static int min(int a, int b) { return a < b ? a : b; }
    private static int max(int a, int b) { return a > b ? a : b; }

    public static int[] prefixFunction(String string) {
        int[] prefix = new int[string.length()];
        prefix[0] = 0;
        for (int i = 1; i < prefix.length; i++) {
            int t = prefix[i - 1];
            while (t > 0 && string.charAt(i) != string.charAt(t)) {
                t = prefix[t - 1];
            }
            if (t > 0) {
                if (string.charAt(i) == string.charAt(t)) {
                    t++;
                }
            } else if (string.charAt(0) == string.charAt(i)) t++;
            else t = 0;
            prefix[i] = t;
        }
        return prefix;
    }

    public static int[] zFunction(String string) {
        int[] zf = new int[string.length()];
        int l = 0, r = 0;
        for (int i = 1; i < string.length(); i++) {
            zf[i] = max(0, min(r - i, zf[i - l]));
            while (i + zf[i] < string.length() && string.charAt(zf[i]) == string.charAt(i + zf[i])) {
                zf[i]++;
            }
            if (i + zf[i] >= r) {
                l = i;
                r = i + zf[i];
            }
        }
        return zf;
    }
}
