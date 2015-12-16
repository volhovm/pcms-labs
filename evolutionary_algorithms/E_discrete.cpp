#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctgmath>
using namespace std;

struct aut_node {
    int left, right;
    vector<double> lc, rc;

    aut_node(int l, int r): left(l), right(r), lc(26,0), rc(26,0)
    {}
};


int main() {
        ios::sync_with_stdio(0);
        ifstream fin("discrete.in");
        ofstream fout("discrete.out");

        int n, m;
        fin >> n >> m;

        vector<aut_node> aut;

        for (int i =0 ; i< n; i++) {
            int a, b;
            fin >> a >> b;
            a--; b--;
            aut.emplace_back(a,b);
        }

        for (int i = 0; i < m; i++) {
            int l;
            string str, out;
            fin >> l >> str >> out;

            double lm1 = ((double)1)/((double)l);
            int current_state = 0;
            for (int j = 0; j < l; j++) {
                if (str[j] == '0') {
                    aut[current_state].lc[out[j]-'a'] += lm1;
                    current_state = aut[current_state].left;
                } else {
                    aut[current_state].rc[out[j]-'a'] += lm1;
                    current_state = aut[current_state].right;
                }
            }
        }

        for (int i = 0; i < n; i++) {
            aut_node n = aut[i];

            double lmax=0;
            int lind=0;
            for (int j = 0; j < 26; j++) {
                if (n.lc[j] > lmax) {
                    lmax = n.lc[j];
                    lind = j;
                }
            }

            double rmax=0;
            int rind=0;
            for (int j = 0; j < 26; j++) {
                if (n.rc[j] > rmax) {
                    rmax = n.rc[j];
                    rind = j;
                }
            }

            fout << ((char)(lind + 'a')) << " " << ((char)(rind + 'a')) << endl;
        }

        fout.close();
}
