#include <stdlib.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
#include <set>
using namespace std;

#define GRAMMAR_SIZE 26

struct automaton {
    int n, s;
    vector<vector<int> > edges;
    unordered_set<int> terminal;
};

unordered_set<int> reachable_vertices(automaton aut) {
    vector<bool> used(aut.n, false);
    unordered_set<int> reachable;
    queue<int> bfsq;

    bfsq.push(aut.s);
    used[aut.s] = true;
    reachable.insert(aut.s);
    while (!bfsq.empty()) {
        int curstate = bfsq.front();
        bfsq.pop();
        for (int i = 0; i < GRAMMAR_SIZE; i++) {
            int u = aut.edges[curstate][i];
            if (!used[u]) {
                reachable.insert(u);
                used[u] = true;
                bfsq.push(u);
            }
        }
    }
    return reachable;
}

vector<vector<bool> > get_marked_table(unordered_set<int> reachable,
                                       automaton aut,
                                       vector<vector<vector<int> > > backedges
                                       ) {
    vector<vector<bool> > marked(aut.n + 1, vector<bool>(aut.n + 1, false));
    queue<pair<int,int> > markedq;

    for (int i = 0; i < aut.n; i++) {
        for (int j = i; j < aut.n; j++) {
            if (marked[i][j]) continue;
            bool a = aut.terminal.count(i) > 0;
            bool b = aut.terminal.count(j) > 0;
            if (a ^ b) {
                marked[i][j] = marked[j][i] = true;
                markedq.push(pair<int,int>(i, j));
                if (i != j) markedq.push(pair<int,int>(j, i));
            }
        }
    }

    while (!markedq.empty()) {
        pair<int,int> curpair = markedq.front();
        int u = curpair.first;
        int v = curpair.second;
        markedq.pop();
        for (int c = 0; c < GRAMMAR_SIZE; c++) {
            for (auto r : backedges[u][c]) {
                for (auto s : backedges[v][c]) {
                    if (!marked[r][s]) {
                        marked[r][s] = marked[s][r] = true;
                        markedq.push(pair<int, int>(r, s));
                    }
                }
            }
        }
    }

    return marked;
}

int main() {
    ifstream fin("minimization.in");
    ofstream fout("minimization.out");

    automaton aut;
    int n, m, k;
    fin >> n >> m >> k;

    // init
    aut.n = n + 1;
    aut.edges.resize(aut.n);
    aut.s = 1;
    for (int i = 0 ; i < aut.n; i++) {
        aut.edges[i].resize(GRAMMAR_SIZE);
        for (int j = 0; j < GRAMMAR_SIZE; j++) aut.edges[i][j] = 0;
    }

    // input
    for (int i = 0; i < k; i++) {
        int termvalue;
        fin >> termvalue;
        aut.terminal.insert(termvalue);
    }
    for (int i = 0; i < m; i++) {
        int a, b;
        char c;
        fin >> a >> b >> c;
        // a--;  // we have additional '0' vertex
        aut.edges[a][c - 'a'] = b;
    }


    // backedges[from][char] == all vertices {to} that edges[to][char] = from
    vector<vector<vector<int> > > backedges(aut.n, vector<vector<int> >(GRAMMAR_SIZE));
    for (int i = 0; i < aut.n; i++) {
        for (int c = 0; c < GRAMMAR_SIZE; c++) {
            for (int j = 0; j < aut.n; j++) {
                if (aut.edges[j][c] == i)
                    backedges[i][c].push_back(j);
            }
        }
    }

    unordered_set<int> reachable = reachable_vertices(aut);
    vector<vector<bool> > marked = get_marked_table(reachable, aut, backedges);

    vector<int> component(aut.n, -1);
    for (int i = 0; i < aut.n; i++) {
        if (!marked[0][i]) {
            component[i] = 0;
        }
    }

    int components_count = 0;
    for (int i = 0; i < aut.n; i++) {
        if (reachable.count(i) == 0) continue;
        if (component[i] == -1) {
            components_count++;
            component[i] = components_count;
            for (int j = i + 1; j < aut.n; j++) {
                if (!marked[i][j]) component[j] = components_count;
            }
        }
    }

    // build minaut
    bool stock_reachable = reachable.count(0) > 0;
    automaton autmin;
    autmin.n = components_count + 1;
    autmin.edges.resize(autmin.n);
    autmin.s = component[aut.s];
    for (int i = 0 ; i < autmin.n; i++) {
        autmin.edges[i].resize(GRAMMAR_SIZE);
        for (int j = 0; j < GRAMMAR_SIZE; j++) autmin.edges[i][j] = -1;
    }

    for (auto t : aut.terminal) {
        if (reachable.count(t) > 0)
            autmin.terminal.insert(component[t]);
    }
    for (int i = 0; i < aut.n; i++) {
        for (int c = 0; c < GRAMMAR_SIZE; c++) {
            if (reachable.count(i) > 0) {
                autmin.edges[component[i]][c] = component[aut.edges[i][c]];
            }
        }
    }

    // output
    if (stock_reachable) {
        // general-purpose
        int autminm = 0;
        for (int i = 0; i < autmin.n; i++) {
            for (int c = 0; c < GRAMMAR_SIZE; c++) {
                if (autmin.edges[i][c] == 0) continue;
                if (autmin.edges[i][c] == -1) continue;
                autminm++;
            }
        }

        fout << autmin.n - 1 << " "
             << autminm << " "
             << autmin.terminal.size() << endl;
        for (auto t : autmin.terminal) fout << t << " ";
        fout << endl;
        for (int i = 0; i < autmin.n; i++) {
            for (int c = 0; c < GRAMMAR_SIZE; c++) {
                if (autmin.edges[i][c] == 0) continue;
                if (autmin.edges[i][c] == -1) continue;
                fout << i << " "
                     << autmin.edges[i][c] << " "
                     << (char) (c + 'a') << endl;
            }
        }
        fout << endl;
    } else {
        //count autmin.m
        int autminm = 0;
        for (int i = 0; i < autmin.n; i++) {
            for (int c = 0; c < GRAMMAR_SIZE; c++) {
                if (autmin.edges[i][c] == -1) continue;
                autminm++;
            }
        }

        fout << autmin.n - 1 << " "
             << autminm << " "
             << autmin.terminal.size() << endl;
        for (auto t : autmin.terminal) fout << (t == 0 ? autmin.n : t) << " ";
        fout << endl;
        for (int i = 0; i < autmin.n; i++) {
            for (int c = 0; c < GRAMMAR_SIZE; c++) {
                if (autmin.edges[i][c] == -1) continue;
                fout << (i == 0 ? autmin.n : i) << " "
                     << (autmin.edges[i][c] == 0 ? autmin.n :
                         autmin.edges[i][c]) << " "
                     << (char) (c + 'a') << endl;
            }
        }
        fout << endl;
    }

    fout.close();
}
