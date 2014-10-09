#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<pair<int, int> > g[20005];
bool used[20005];
int timer, tin[20005], fup[20005];
vector<bool> out;
vector<int> comps;
int compscounter = 0;

void dfs (int v, int p = -1) {
	used[v] = true;
	tin[v] = fup[v] = timer++;
	for (size_t i=0; i<g[v].size(); i++) {
		int to = g[v][i].first;
		if (to == p)  continue;
		if (used[to])
			fup[v] = min (fup[v], tin[to]);
		else {
			dfs (to, v);
			fup[v] = min (fup[v], fup[to]);
					if (fup[to] > tin[v])
						out[g[v][i].second] = true;
		}
	}
}

void find_bridges() {
	timer = 0;
	for (int i=0; i<n; ++i)
		if (!used[i])
			dfs (i);
}

void dfs2(int v, int counter) {
	used[v] = true;
	comps[v] = counter;
	for (int i = 0; i < g[v].size(); i++) {
		int to = g[v][i].first;
		if (!used[to]){
			int toE = g[v][i].second;
//			cout << "~" << toE << endl;
			if (out[toE]) {
				compscounter++;
				dfs2(to, compscounter);
			} else dfs2(to, counter);
		}
	}
}

void find_comps() {
	for (int i = 0; i < n; i++) {
		if (!used[i]){
			compscounter++;
			dfs2(i, compscounter);
		}
	}
}

int main() {
	ifstream fin("bicone.in");
    ios_base::sync_with_stdio(0);
	ofstream fout("bicone.out");
	fin >> n >> m;
	comps.resize(n);
	out.resize(m);
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b; 
		g[a - 1].push_back(pair<int, int>(b - 1, i));
		g[b - 1].push_back(pair<int, int>(a - 1, i));
	}
	find_bridges();
	for (int i = 0; i < n; i++) {
		used[i] = false;
	}
//	for (int i = 0; i < m; i++) {
//		cout << out[i] << " ";
//	}
//	cout << endl;
	find_comps();
	fout << compscounter << endl;
	for (int i = 0; i < n; i++) fout << comps[i] << " ";
	fout.close();
}
