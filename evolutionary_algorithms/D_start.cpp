#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctgmath>
using namespace std;

struct aut_node {
    int left, right;
    char c;

    aut_node(int l, int r, int ch): left(l), right(r), c(ch)
    {}
};

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("start.in");
        ofstream fout("start.out");

        vector<aut_node> aut;

        int n, m;
        fin >> m >> n;

        for (int i =0 ; i< n; i++) {
            int a, b;
            char c;
            fin >> a >> b >> c;
            a--; b--;
            aut.emplace_back(a,b,c);
        }

        string z;
        fin >> z;

        // d[i][j] -- можно ли из i-го элемента вывести суффикс
        // строки z[j..m-1]?
        vector<vector<bool> > d(n, vector<bool>(m, false));

        for (int i = 0; i < n; i++) {
            if ((aut[aut[i].left].c == z[m-1])
             || (aut[aut[i].right].c == z[m-1])
                ) {
                d[i][m-1] = true;
            }
        }

        for (int j = m-2; j >= 0; j--) {
            for (int i = 0; i < n; i++) {
                if ((d[aut[i].left][j+1] && aut[aut[i].left].c == z[j])
                 || (d[aut[i].right][j+1] && aut[aut[i].right].c == z[j])
                    ) {
                    d[i][j] = true;
                }
            }
        }

        vector<int> solution;
        for (int i = 0; i < n; i++) {
            if (d[i][0]) solution.push_back(i+1);
        }

        fout << solution.size() << " ";
        for (int a : solution) { fout << a << " "; }

        fout.close();
}
