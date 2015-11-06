// zahodi, ept
#include <stdlib.h>
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

struct automaton {
    int size;
    int start;
    vector<vector<vector<int>> > trans;
    set<int> term;
    automaton(int n) : trans(n), size(n) {
        for (int i = 0; i < n; i++) trans[i].resize(SMALL_ALPHABET_SIZE);
    }
};

bool resolves(automaton* a, string word) {
    int position = 0;
    queue<int> current_vertices;
    queue<int> other;
    current_vertices.push(a->start);

    while (position < word.size()) {
        while (!other.empty()) {
            other.pop();
        }
        while (!current_vertices.empty()) {
            other.push(current_vertices.front());
            current_vertices.pop();
        }
        while (!other.empty()) {
            int state = other.front();
            other.pop();
            int curchar = word[position] - 'a';
            vector<int> nexts = a->trans[state][curchar];
            for (int j = 0; j < nexts.size(); j++) {
                current_vertices.push(nexts[j]);
            }
            //cerr << "current state is: " << position << " " << state << " "
            //     << next << " " << word[position] << endl;
        }
        position++;
    }
    while (!current_vertices.empty()) {
        int curr = current_vertices.front();
        current_vertices.pop();
        if (a->term.count(curr) > 0) return true;
    }
    return false;
}

int main() {
    ifstream fin("automaton.in");
    ofstream fout("automaton.out");

    automaton grammaut(BIG_ALPHABET_SIZE + 1);
    // there will be the only terminating vertex -- the last one
    grammaut.term.insert(BIG_ALPHABET_SIZE);

    int m;
    char S;
    fin >> m >> S;
    grammaut.start = S - 'A';

    for (int i = 0; i < m; i++) {
        char from;
        string to, whatever;
        fin >> from >> whatever >> to;
        grammaut.trans[from - 'A'][to[0] - 'a']
            .push_back(to.size() == 1 ? BIG_ALPHABET_SIZE : (to[1] - 'A'));
    }

    int k;
    fin >> k;

    for (int i = 0; i < k; i++) {
        string curword;
        fin >> curword;
        //        cerr << "Processing " << i+1 << " of " << k << endl;
        if (resolves(&grammaut, curword)) fout << "yes" << endl;
        else fout << "no" << endl;
    }

    fout.close();
}
