#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<int> g[20005];
bool used[20005];
int timer, tin[20005], fup[20005];
vector<int> out;

void proceed(int v) {
	if(find(out.begin(), out.end(), v) == out.end()) {
		out.push_back(v); 
	}
}

void dfs (int v, int p = -1) {
	used[v] = true;
	tin[v] = fup[v] = timer++;
	int children = 0;
	for (size_t i=0; i<g[v].size(); ++i) {
		int to = g[v][i];
		if (to == p)  continue;
		if (used[to])
			fup[v] = min (fup[v], tin[to]);
		else {
			dfs (to, v);
			fup[v] = min (fup[v], fup[to]);
			if (fup[to] >= tin[v] && p != -1)
				proceed(v);
			++children;
		}
	}
	if (p == -1 && children > 1)
		proceed(v);	
}

void find_points() {
	timer = 0;
	for (int i = 0; i < n; i++) {
		used[i] = false;
	}
	dfs (0);
}

int main() {
	ifstream fin("points.in");
	ofstream fout;
	fout.open("points.out");
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b; 
		g[a - 1].push_back(b - 1);
		g[b - 1].push_back(a - 1);
	}
	find_points();
	fout << out.size() << endl;
	sort(out.begin(), out.end());
	for (int i = 0; i < out.size(); i++) {
		fout << out[i] + 1 << endl;
	}
	fout.close();
}
