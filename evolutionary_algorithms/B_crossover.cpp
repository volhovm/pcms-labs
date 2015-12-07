#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctgmath>
using namespace std;

bool c1, c2, c3;
int n, m;
string s;

void checkStrings(string& s1, string& s2) {
    int swapsCount = 0;
    int curDom = 1;
    bool failed = false;
    for (int k = 1; k < n; k++) {
        if ((s[k] == s1[k] && s[k] != s2[k] && curDom == 2)
            ||
            (s[k] == s2[k] && s[k] != s1[k] && curDom == 1)) {
            swapsCount++;
            curDom = curDom == 1 ? 2 : 1;
        }

        if (s[k] != s1[k] && s[k] != s2[k]) {
            return;
        }
    }

    c3 = true;
    if (swapsCount <= 2) c2 = true;
    if (swapsCount <= 1) c1 = true;

    cerr << "State: " << swapsCount << " " << c1 << " "
         << c2 << " " << c3 << endl;
}

int main() {
    ios::sync_with_stdio(0);
        ifstream fin("crossover.in");
        ofstream fout("crossover.out");

        fin >> m >> n;

        vector<string> inputs(m);
        vector<bool> isCrossover(m);

        for (int i = 0; i < m; i++) {
            string curr;
            fin >> curr;
            inputs[i] = curr;
        }
        fin >> s;

        vector<bool> repeats(m, false);

        for (int i = 0 ; i < m && !c1; i++) {
            for (int j = i; j < m && !c1; j++) {
                if (i == j && repeats[i]) continue;
                else repeats[i] = true;

                string s1 = inputs[i];
                string s2 = inputs[j];

                cerr << "Processing: " << s1 << " : " << s2 << endl;

                // Can't initialize one-two crossover search
                if (s[0] != s1[0] && s[0] != s2[0]) {
                    continue;
                }

                if (s[0] == s1[0] && s[0] == s2[0]) {
                    checkStrings(s1, s2);
                    checkStrings(s2, s1);
                } else if (s[0] == s1[0]) {
                    checkStrings(s1, s2);
                } else {
                    checkStrings(s2, s1);
                }

            }
        }

        fout << (c1 ? "YES" : "NO") << endl;
        fout << (c2 ? "YES" : "NO") << endl;
        fout << (c3 ? "YES" : "NO") << endl;

        fout.close();
}
