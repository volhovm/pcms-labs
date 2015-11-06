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

int main() {
    ifstream fin("epsilon.in");
    ofstream fout("epsilon.out");

    int n;
    char start;
    fin >> n >> start;
    string whatever;
    getline(fin, whatever);

    vector<rule> rules;
    bool is_epsilon_deriving[26];
    for (int i = 0; i < 26; i++) is_epsilon_deriving[i] = false;

    for (int i = 0; i < n; i++) {
        rule curr;
        fin >> curr;
        bool small_found = false;
        for (int i = 0; i < curr.resolvers.size() && !small_found; i++) {
            if (curr.resolvers[i] < 'A' || curr.resolvers[i] > 'Z') small_found = true;
        }
        if (!small_found) {
            rules.push_back(curr);
            if (curr.resolvers.empty()) {
                is_epsilon_deriving[rules.back().termvalue - 'A'] = true;
            }
        }
    }

    for (int counter = 0; counter < n + 5; counter++) {
        for (int i = 0; i < rules.size(); i++) {
            rule curr = rules[i];

            char rulefrom = curr.termvalue - 'A';
            if (is_epsilon_deriving[rulefrom]) continue;

            bool all_eps = true;
            for (int j = 0; j < curr.resolvers.size() && all_eps; j++) {
                char subchar = curr.resolvers[j];
                if (subchar < 'A' || subchar > 'Z') all_eps = false;
                else if (!is_epsilon_deriving[subchar - 'A'])
                    all_eps = false;
            }

            if (all_eps) {
                is_epsilon_deriving[rulefrom] = true;
            }
        }
    }

    for (int i = 0 ; i < 26; i++) {
        if (is_epsilon_deriving[i]) {
            fout << (char) (i + 'A') << " ";
            cerr << (char) (i + 'A') << " ";
        }
    }

    fout.close();
}
