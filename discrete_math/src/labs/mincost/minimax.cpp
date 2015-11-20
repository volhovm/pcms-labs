#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <algorithm>
using namespace std;

int n = 0, max_e = -1;
int matrix[301][301];
vector<int> mt;
vector<char> used_kuhn;

bool kuhn (int v, int aver) {
    if (used_kuhn[v]) return false;
    //	cout << "kuhn in vertex " << v << endl;
    used_kuhn[v] = true;
    for (int to = 0; to < n; to++) {
	if (matrix[v][to] >= aver)
	    if (mt[to] == -1 || kuhn (mt[to], aver) ) {
		mt[to] = v;
		return true;
	    }
    }
    return false;
}

bool check_kunh(int aver) {
    mt.assign (n, -1);
    for (int v = 0; v < n; v++) {
	used_kuhn.assign(n, false);
	kuhn (v, aver);
    }

    for (int i = 0; i < n; i++){
	cout << mt[i] << " - " << i << endl;
	if (mt[i] == -1) return false;
    }
    return true;
}


int bin_search(int from, int to) {
    if (from >= to) return -1;
    if (from + 1 == to) return from;
    int aver = (from + to) / 2;
    //    cout << from << " :" << aver << ": " << to << endl;
    if (check_kunh(aver)) {
	return bin_search(aver, to);
    }
    else {
	return bin_search(from, aver);
    }
}

int main() {
    ios::sync_with_stdio(0);
    ifstream fin("minimax.in");
    ofstream fout;
    fout.open("minimax.out");
    fin >> n;
    for (int i = 0; i < n; i++)
	for (int j = 0; j < n; j++) {
	    fin >> matrix[i][j];
	    max_e = max(max_e, matrix[i][j]);
	}
    fout << bin_search(0, max_e + 1);
    fout.close();
}
