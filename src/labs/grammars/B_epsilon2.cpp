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

#define BIG_ALPHABET_SIZE 26
#define SMALL_ALPHABET_SIZE 26

//data elem = terminal int | nonterminal int

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
int is_epsilon_deriving[GRAMMAR_SIZE];

// 2 -> 2
// 1 -> 1
// -1 -> -1
// 0 -> somewhat
void fill_nonterminal(int nonterminal) {
    if (is_epsilon_deriving[nonterminal] == 2) {
        return;
    }
    if (is_epsilon_deriving[nonterminal] == 1 ||
        is_epsilon_deriving[nonterminal] == -1) return;

    // used flag
    is_epsilon_deriving[nonterminal] = 2;

    vector<rule> rulevec = rules[nonterminal];

    if (rulevec.empty()) {
        is_epsilon_deriving[nonterminal] = -1;
        return;
    }

    int queueskips = 0;
    queue<rule> rulequeue;
    for (int i = 0; i < rulevec.size(); i++) {
        rulequeue.push(rulevec[i]);
    }

    while (!rulequeue.empty() && queueskips < rulequeue.size()) {
        rule currule = rulequeue.front();
        bool bad_rule = false;

        // loop over chars
        queue<int> innerqueue;
        int innerqueueskips = 0;

        for (int j = 0; j < currule.resolvers.size(); j++) {
            innerqueue.push(currule.resolvers[j] - 'A');
        }
        while (!innerqueue.empty() && !bad_rule &&
               innerqueueskips < innerqueue.size()) {
            int curelem = innerqueue.front();
            if (curelem < 0 || curelem > 25) {
                bad_rule = true;
            } else {
                fill_nonterminal(curelem);
                if (is_epsilon_deriving[curelem] == -1) bad_rule = true;
                else if (is_epsilon_deriving[curelem] == 2) {
                    innerqueueskips++;
                    innerqueue.push(curelem);
                    innerqueue.pop();
                    continue;
                }
            }
            innerqueue.pop();
            innerqueueskips = 0;
        }

        if (bad_rule) {
            rulequeue.pop();
            queueskips = 0;
        } else if (!innerqueue.empty() &&
                   innerqueueskips >= innerqueue.size()) {
            rulequeue.push(currule);
            rulequeue.pop();
            queueskips++;
        } else {
            // we got eps rule, yep!
            is_epsilon_deriving[nonterminal] = 1;
            return;
        }
    }
    if (queueskips >= rulequeue.size()) {
        // kind of recursion.. eh
        // vertex can't be epsilon-producing if it's every rule
        // depends on something recursive
        is_epsilon_deriving[nonterminal] = -1;
    } else {
        // just no suitable rules, life is life
        is_epsilon_deriving[nonterminal] = -1;
    }
}

void fill_epsilon_table() {
    for (int i = 0; i < GRAMMAR_SIZE; i++) is_epsilon_deriving[i] = 0;

    // initial epsilon-producing nonterminals
    for (int i = 0; i < GRAMMAR_SIZE; i++) {
        for (int j = 0 ; j < rules[i].size(); j++) {
            rule curr = rules[i][j];
            if (curr.resolvers.empty())
                is_epsilon_deriving[curr.termvalue - 'A'] = 1;
        }
    }

    for (int i = 0; i < GRAMMAR_SIZE; i++) {
        fill_nonterminal(i);
    }
}

int main() {
    ifstream fin("epsilon.in");
    ofstream fout("epsilon.out");

    int n;
    char start;
    fin >> n >> start;
    string whatever;
    getline(fin, whatever);

    for (int i = 0; i < n; i++) {
        rule curr;
        fin >> curr;
        bool small_found = false;
        for (int i = 0; i < curr.resolvers.size() && !small_found; i++) {
            if (curr.resolvers[i] < 'A' || curr.resolvers[i] > 'Z') small_found = true;
        }
        if (!small_found) {
            rules[curr.termvalue - 'A'].push_back(curr);
        }
    }

    fill_epsilon_table();

    for (int i = 0 ; i < 26; i++) {
        if (is_epsilon_deriving[i] == 1) {
            fout << (char) (i + 'A') << " ";
            cerr << (char) (i + 'A') << " ";
        }
    }
    cerr << endl;

    fout.close();
}
