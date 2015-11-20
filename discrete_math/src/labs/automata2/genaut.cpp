#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <set>
#include <utility>
using namespace std;

#define GRAMMAR_SIZE 26

struct edge {
    int from, to, c;

    bool operator<(edge const b) const {
        if (from < b.from) return true;
        if (c < b.c) return true;
        return false;
    }

    bool operator==(edge const b) const {
        return from == b.from &&
            c == b.c;
    }
};

int main() {
    ofstream fout("minimization_big.in");
    srand (time(NULL));

    int n = 50000;
    int m = 50000;
    int k = 3000;

    int currn = 0;

    unordered_set<int> terminals;
    while (terminals.size() < k) {
        terminals.insert((rand() % n) + 1);
    }

    set<edge> edges;
    while (edges.size() < m) {
        edge e;
        e.from = rand() % n + 1;
        e.to = rand() % n + 1;
        e.c = rand() % 26;
        edges.insert(e);
    }

    fout << n << " " << m << " " << k << endl;
    for (int t : terminals) fout << t << " ";
    fout << endl;
    for (edge e : edges) {
        fout << e.from << " " << e.to << " " << (char) (e.c + 'a') << endl;
    }

    fout.close();
}
