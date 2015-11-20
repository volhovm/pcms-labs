#include <stdlib.h>
#include <string.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <string>
#include <unordered_set>
#include <set>
using namespace std;


struct rule {
    int termvalue;
    vector<int> resolvers;
};

ifstream& operator>>(ifstream& in, rule& r) {
    int position = 1;
    string s;
    getline(in, s);

    r.termvalue = s[0] - 'A';

    while (position < s.size() &&
           (s[position] == '-' || s[position] == '>' || s[position] == ' '))
        position++;

    while (position < s.size()) {
        r.resolvers.push_back(s[position]);
        position++;
    }

    return in;
}

#define GRAMMAR_SIZE 26
#define MAXM 150
#define MOD 1000000007

int m;
string target;

vector<vector<rule> > rulesAB(GRAMMAR_SIZE);
vector<vector<rule> > rulesa(GRAMMAR_SIZE);
int start_terminal;
long long int dynamics[GRAMMAR_SIZE][MAXM][MAXM];

int main() {
    ifstream fin("cf.in");
    ofstream fout("cf.out");

    int n;
    char start;
    fin >> n >> start;
    start_terminal = (int) (start - 'A');

    string whatever;
    getline(fin, whatever);

    for (int i = 0; i < n; i++) {
        rule curr;
        fin >> curr;
        if (curr.resolvers.size() == 2) {
            rulesAB[curr.termvalue].push_back(curr);
        } else if (curr.resolvers.size() == 1) {
            rulesa[curr.termvalue].push_back(curr);
        } else return 1;
    }

    fin >> target;
    m = target.size();

    /// INIT
    // diagonal members
    for (int nonterm = 0; nonterm < GRAMMAR_SIZE; nonterm++) {
        for (int j = 0; j < m; j++) {
            dynamics[nonterm][j][1] = 0;
            for (int i = 0; i < rulesa[nonterm].size(); i++) {
                char c = rulesa[nonterm][i].resolvers[0];
                if (target[j] == c) {
                    dynamics[nonterm][j][1] = 1;
                    break;
                }
            }
        }
    }

    /// COMPUTE
    for (int len = 2; len <= m; len++) {
        for (int i = 0; i <= m - len; i++) {
            for (int nonterm = 0; nonterm < GRAMMAR_SIZE; nonterm++) {
                long long int accum = 0;
                for (int j = 0; j < rulesAB[nonterm].size(); j++) {
                    rule currule = rulesAB[nonterm][j];
                    for (int k = 1; k < len; k++) {
                        int a = currule.resolvers[0] - 'A';
                        int b = currule.resolvers[1] - 'A';
                        accum += ((dynamics[a][i][k] % MOD) *
                                  (dynamics[b][i+k][len-k] % MOD)) % MOD;
                        accum %= MOD;
                    }
                }
                dynamics[nonterm][i][len] = accum % MOD;
            }
        }
    }

    long long int res = dynamics[start_terminal][0][m] % MOD;

    fout << res << endl;
    cerr << res << endl;

    fout.close();
}
