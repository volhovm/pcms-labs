package labs.sortsearch;

/**
 * @author volhovm
 *         Created on 5/25/14
 */
public class Builder {
    public static void main(String[] args) {
        String s = "[[0,1],[2,3],[4,5],[6,7],[8,9],[10,11],[12,13],[14,15]]\n" +
                "[[0,2],[4,6],[8,10],[12,14],[1,3],[5,7],[9,11],[13,15]]\n" +
                "[[0,4],[8,12],[1,5],[9,13],[2,6],[10,14],[3,7],[11,15]]\n" +
                "[[0,8],[1,9],[2,10],[3,11],[4,12],[5,13],[6,14],[7,15]]\n" +
                "[[5,10],[6,9],[3,12],[13,14],[7,11],[1,2],[4,8]]\n" +
                "[[1,4],[7,13],[2,8],[11,14],[5,6],[9,10]]\n" +
                "[[2,4],[11,13],[3,8],[7,12]]\n" +
                "[[6,8],[10,12],[3,5],[7,9]]\n" +
                "[[3,4],[5,6],[7,8],[9,10],[11,12]]\n" +
                "[[6,7],[8,9]]";
        String[] a = s.replace('[', ' ').replace(']', ' ').replace(',', ' ').replaceAll(" +", " ").split("\\n");
        for (int i = 0; i < a.length; i++) {
            String[] x = a[i].trim().split(" +");
            for (int j = 0; j < x.length; j++) {
                x[j] = String.valueOf(Integer.parseInt(x[j]) + 1);
                System.out.print(x[j] + " ");
            }
            System.out.println();
        }
    }
}
