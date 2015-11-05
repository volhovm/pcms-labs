#include <stdlib.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

// OK

typedef vector<int*> aut;
typedef set<int> term;

bool dfs(bool *visited,
        aut *aut1, aut *aut2,
        term *term1, term *term2,
        int v1, int v2) {
    visited[v1] = true;
    if (term1->count(v1) != term2->count(v2)) {
        return false;
    }

    // no edges
    if ((*aut1)[v1] == NULL && (*aut2)[v2] == NULL) return true;
    // one vertix has no edges
    if ((*aut1)[v1] == NULL || (*aut2)[v2] == NULL) return false;

    bool res = true;
    for (int i = 0; i < 27 && res; i++) {
        if ((*aut1)[v1][i] == -1) continue;
        if ((*aut2)[v2][i] == -1) return false;

        if (!visited[(*aut1)[v1][i]])
            res &= dfs(visited,
                       aut1, aut2,
                       term1, term2,
                       (*aut1)[v1][i],
                       (*aut2)[v2][i]);
    }
    return res;
}

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("isomorphism.in");
        ofstream fout;
        fout.open("isomorphism.out");

        int n1, m1, k1, n2, m2, k2;

        fin >> n1 >> m1 >> k1;

        term terminal1;
        for (int i = 0; i < k1; i++) {
            int curr;
            fin >> curr;
            curr--;
            terminal1.insert(curr);
        }

        aut aut1(m1);
        for (int i = 0; i < m1; i++) {
            int a, b;
            char c;
            fin >> a >> b >> c;
            a--;
            b--;
            if (aut1[a] == NULL) {
                aut1[a] = (int*) malloc(27 * sizeof(int));
                for (int i = 0; i < 27; i++) aut1[a][i] = -1;
            }
            aut1[a][c - 'a'] = b;
        }

        fin >> n2 >> m2 >> k2;

        if (n1 != n2 || m1 != m2) {
            fout << "NO" << endl;
            return 0;
        }

        term terminal2;
        for (int i = 0; i < k2; i++) {
            int curr;
            fin >> curr;
            curr--;
            terminal2.insert(curr);
        }

        aut aut2(m2);
        for (int i = 0; i < m2; i++) {
            int a, b;
            char c;
            fin >> a >> b >> c;
            a--;
            b--;
            if (aut2[a] == NULL) {
                aut2[a] = (int*) malloc(27 * sizeof(int));
                for (int i = 0; i < 27; i++) aut2[a][i] = -1;
            }
            aut2[a][c - 'a'] = b;
        }

        bool visited[n1];
        for (int i = 0; i < n1; i++) visited[i] = false;
        bool result = dfs(visited,
                          &aut1, &aut2,
                          &terminal1, &terminal2,
                          0, 0);

        fout << (result ? "YES" : "NO") << endl;
        cout << (result ? "YES" : "NO") << endl;

        fout.close();
}
