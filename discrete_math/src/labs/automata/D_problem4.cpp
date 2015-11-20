// NOT WORKING
#include <stdlib.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

int n;

long long** mult(long long** arr1, long long** arr2) {
    long long** newarr = (long long**) malloc(n * n * sizeof (long long));

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            newarr[i][j] = 0;
            long long accum = 0;
            for (int u = 0; u < n; u++) {
                accum += arr1[i][u] * arr2[u][j];
            }
            newarr[i][j] = accum % 1000000007;
        }
    }

    return newarr;
}

long long** exp(long long** arr1, int q) {
    if (q == 1) return arr1;
    if (q % 2 == 1) {
        return mult(exp(arr1, q-1), arr1);
    } else {
        long long** newarr = exp(arr1, q/2);
        return mult(newarr, newarr);
    }
}

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("problem4.in");
        ofstream fout;
        int m, k, l;
        fout.open("problem4.out");
        fin >> n >> m >> k >> l;
        bool isTerminal[n];
        for (int i = 0; i < n; i++) { isTerminal[i] = false; }
        for (int i = 0; i < k; i++) {
            int term;
            fin >> term;
            isTerminal[term] = true;
        }

        long long** initarray = (long long**) malloc(n*n*sizeof(long long));
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                initarray[i][j] = 0;
            }
        }

        for (int i = 0; i < m; i++) {
            int a, b;
            char c;
            fin >> a >> b >> c;
            initarray[a-1][b-1]++;
        }

        long long** readyarr = exp(initarray, l);

        long long accum = 0;
        for (int i = 0; i < k; i++) {
            accum += readyarr[0][isTerminal[n]] % 1000000007;
        }

        fout << accum << endl;

        fout.close();
}
