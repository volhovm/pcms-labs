#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

enum Action { L, R, M };

struct Edge {
    int from, to;
    Action action;
};

struct Automaton {
    int startState;
    vector<vector<Edge> > edges;

};

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("$$name$$.in");
        ofstream fout;
        fout.open("$$name$$.out");

        fout.close();
}
