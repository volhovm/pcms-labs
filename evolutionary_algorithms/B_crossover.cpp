#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctgmath>
using namespace std;

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("crossover.in");
        ofstream fout("crossover.out");

        int n, m;
        fin >> n >> m;
        double f = ((double) 1)/ n;
        for (int i =0 ; i < m; i++) {
            string a, b;
            fin >> a >> b;

            int diffs = 0;
            for (int i = 0; i < n; i++) {
                if (a[i] != b[i]) diffs++;
            }
            double result = pow(f, diffs) * pow(1 - f, n - diffs);
            fout << setprecision(9) << fixed << result << endl;
        }

        fout.close();
}
