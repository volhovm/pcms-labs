#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <algorithm>
using namespace std;

int n;
vector<int> g[100000];
int visited[100000]; //0 white, 1 grey, 2 black
bool invalid = false;
vector<int> out;

void dfs(int v) {
	visited[v] = 1;
	for (size_t i = 0; i < g[v].size(); i++) {
		int to = g[v][i];
		if (visited[to] == 0) {
			dfs (to);
		} else if (visited[to] == 1) {
			invalid = true;
			return;
		}
	}
    out.push_back(v);
	visited[v] = 2;
}

void top_sort() {
	for (size_t i = 0; i < n; i++) {
		visited[i] = 0;
	}
	out.clear();
	for (int i = 0; i < n; i++) {
		if (visited[i] == 0) dfs(i);
		if (invalid) return;
	}
	reverse(out.begin(), out.end());
}

int main() {
	ifstream str("topsort.in");
	ofstream strout;
	strout.open("topsort.out");
	int m;
    str >> n >> m;
	out = vector<int>(n);
	for (size_t i = 0; i < m; i++) {
		int a, b;
		str >> a >> b;
		g[a - 1].push_back(b - 1);
	}
	top_sort();
	if (invalid) strout << "-1";
	else for (size_t i = 0; i < out.size(); i++){
		strout << (out[i] + 1) << " ";
	}
	strout.close();
}


	
