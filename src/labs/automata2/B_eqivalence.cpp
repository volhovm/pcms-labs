#include <stdlib.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
#include <set>
using namespace std;

// OK

typedef vector<int*> aut;
typedef set<int> term;

bool bfs(bool *visited1,
         bool *visited2,
         aut *aut1, aut *aut2,
         term *term1, term *term2) {
    queue<pair<int, int> > Q;

    Q.push(pair<int,int>(0, 0));
    visited1[0] = true;
    visited2[0] = true;

    while (!Q.empty()) {
        pair<int, int> curr = Q.front();
        Q.pop();
        int u = curr.first;
        int v = curr.second;
        if (term1->count(u) != term2->count(v)) {
            return false;
        }
        for (int c = 0; c < 26; c++) {
            int x = (*aut1)[u][c];
            int y = (*aut2)[v][c];
            if (!visited1[x] || !visited2[y]) {
                Q.push(pair<int,int>(x, y));
                visited1[x] = true;
                visited2[y] = true;
            }
        }
    }
    return true;
}

int main() {
    ifstream fin("equivalence.in");
    ofstream fout("equivalence.out");

    int n1, m1, k1, n2, m2, k2;

    fin >> n1 >> m1 >> k1;

    term terminal1;
    for (int i = 0; i < k1; i++) {
        int curr;
        fin >> curr;
        curr--;
        terminal1.insert(curr);
    }

    aut aut1(n1 + 10);
    for (int a = 0; a < n1 + 1; a++) {
        aut1[a] = (int*) malloc(27 * sizeof(int));
        for (int i = 0; i < 27; i++) aut1[a][i] = n1;
    }
    for (int i = 0; i < m1; i++) {
        int a, b;
        char c;
        fin >> a >> b >> c;
        a--;
        b--;
        aut1[a][c - 'a'] = b;
    }

    fin >> n2 >> m2 >> k2;

    term terminal2;
    for (int i = 0; i < k2; i++) {
        int curr;
        fin >> curr;
        curr--;
        terminal2.insert(curr);
    }

    aut aut2(n2 + 10);
    for (int a = 0; a < n2 + 1; a++) {
        aut2[a] = (int*) malloc(27 * sizeof(int));
        for (int i = 0; i < 27; i++) aut2[a][i] = n2;
    }
    for (int i = 0; i < m2; i++) {
        int a, b;
        char c;
        fin >> a >> b >> c;
        a--;
        b--;
        aut2[a][c - 'a'] = b;
    }

    bool visited1[n1 + 10];
    bool visited2[n1 + 10];
    for (int i = 0; i < n1 + 10; i++) {
        visited1[i] = false;
        visited2[i] = false;
    }
    bool result = bfs(visited1, visited2,
                      &aut1, &aut2,
                      &terminal1, &terminal2);

    fout << (result ? "YES" : "NO") << endl;
    cout << (result ? "YES" : "NO") << endl;

    fout.close();
}
