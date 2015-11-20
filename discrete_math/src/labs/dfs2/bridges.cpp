#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<pair<int, int>> g[20005];
bool used[20005];
int timer, tin[20005], fup[20005];
vector<int> out;

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
						out.push_back(g[v][i].second);
		}
	}
}

void find_bridges() {
	timer = 0;
	for (int i=0; i<n; ++i)
		if (!used[i])
			dfs (i);

}

int main() {
	ifstream fin("bridges.in");
	ofstream fout;
	fout.open("bridges.out");
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b; 
		g[a - 1].push_back(pair<int, int>(b - 1, i + 1));
		g[b - 1].push_back(pair<int, int>(a - 1, i + 1));
	}
	find_bridges();
	fout << out.size() << endl;
	sort(out.begin(), out.end());
	for (int i = 0; i < out.size(); i++) {
		fout << out[i] << " ";
	}
	fout.close();
}
