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

// OK

struct rule {
    int termvalue;
    vector<int> resolvers;
};

ifstream& operator>>(ifstream& in, rule& r) {
    int position = 1;
    string s;
    getline(in, s);

    r.termvalue = s[0];

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
vector<vector<rule> > rules(GRAMMAR_SIZE);
int start_terminal;
bool nonterminals_used[GRAMMAR_SIZE];
bool generating[GRAMMAR_SIZE];
bool reachable[GRAMMAR_SIZE];

void fill_generating_table() {
    for (int i = 0; i < GRAMMAR_SIZE; i++) {
        generating[i] = false;
        bool nontermfound = false;

        if (rules[i].empty()) continue;
        if (!nonterminals_used[i]) continue;

        for (int j = 0; j < rules[i].size() && !nontermfound; j++) {
            for (int k = 0; k < rules[i][j].resolvers.size() && !nontermfound; k++) {
                int currelem = rules[i][j].resolvers[k] - 'A';
                // found big nonterminal
                if (currelem >= 0 && currelem < GRAMMAR_SIZE) {
                    nontermfound = true;
                }
            }
        }
        if (!nontermfound) {
            generating[i] = true;
        }
    }

    // stop when set willn't change after 1 loop iteration
    bool unchanged = false;
    while (!unchanged) {
        unchanged = true;

        for (int i = 0; i < GRAMMAR_SIZE; i++) {
            // skip if not present in rule set
            if (!nonterminals_used[i]) continue;
            // skip if already generating
            if (generating[i]) continue;
            // we don't need nonterminals with no rules either
            if (rules[i].empty()) continue;

            for (int j = 0; j < rules[i].size(); j++) {
                bool ruleisok = true;
                rule currule = rules[i][j];
                for (int k = 0; k < currule.resolvers.size() && ruleisok; k++) {
                    int currelem = currule.resolvers[k] - 'A';
                    if (currelem >= 0 && currelem < GRAMMAR_SIZE) {
                        if (!generating[currelem]) {
                            ruleisok = false;
                        }
                    }
                }
                if (ruleisok) {
                    unchanged = false;
                    generating[i] = true;
                    break;
                }
            }
        }
    }
}

void fill_reachable_table() {
    // cleanup
    for (int i = 0; i < GRAMMAR_SIZE; i++) reachable[i] = false;

    queue<int> q;

    q.push(start_terminal);
    reachable[start_terminal] = true;

    while (!q.empty()) {
        int current = q.front();
        q.pop();
        for (int i = 0; i < rules[current].size(); i++) {
            rule currule = rules[current][i];
            for (int j = 0; j < currule.resolvers.size(); j++) {
                int curelem = currule.resolvers[j] - 'A';
                if (curelem >= 0 && curelem < GRAMMAR_SIZE &&
                    !reachable[curelem]) {
                    q.push(curelem);
                    reachable[curelem] = true;
                }
            }
        }
    }
}

int main() {
    ifstream fin("useless.in");
    ofstream fout("useless.out");

    int n;
    char start;
    fin >> n >> start;
    start_terminal = (int) (start - 'A');

    string whatever;
    getline(fin, whatever);

    for (int i = 0; i < n; i++) {
        rule curr;
        fin >> curr;
        rules[curr.termvalue - 'A'].push_back(curr);
        nonterminals_used[curr.termvalue - 'A'] = true;
        for (int j = 0; j < curr.resolvers.size(); j++) {
            int currelem = curr.resolvers[j] - 'A';
            if (currelem >= 0 && currelem < GRAMMAR_SIZE)
                nonterminals_used[currelem] = true;
        }
    }

    fill_generating_table();

    for (int i = 0 ; i < GRAMMAR_SIZE; i++) {
        if (nonterminals_used[i]
            && !generating[i]) {
            cerr << (char) (i + 'A') << " ";
        }
    }
    cerr << endl;

    fill_reachable_table();
    for (int i = 0 ; i < GRAMMAR_SIZE; i++) {
        if (nonterminals_used[i]
            && !reachable[i]) {
            cerr << (char) (i + 'A') << " ";
        }
    }
    cerr << endl;
    cerr << "together:" << endl;
    for (int i = 0 ; i < GRAMMAR_SIZE; i++) {
        if (nonterminals_used[i]
            && (!generating[i] || !reachable[i])) {
            fout << (char) (i + 'A') << " ";
            cerr << (char) (i + 'A') << " ";
        }
    }
    cerr << endl;

    fout.close();
}
