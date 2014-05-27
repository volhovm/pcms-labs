package labs.sortsearch;

import java.io.*;

/**
 * @author volhovm
 *         Created on 5/25/14
 */
public class NetTester {
    public static void main(String[] args) throws IOException {
        for (int i = 1; i < 17; i++){
            PrintWriter scout = new PrintWriter(new File("netbuild.in"));
            scout.print(i);
            scout.close();
            H_netBuild.main(null);
            G_netCheck.main(null);
            BufferedReader br = new BufferedReader(new FileReader("netcheck.out"));
            System.out.println(i + " " + br.readLine());
            br.close();
        }
    }
}
