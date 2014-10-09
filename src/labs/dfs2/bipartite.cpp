#include <vector>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <deque>
#include <cstdio>
using namespace std;

int m,n;
vector<int> g[100001];
bool isbi = true;
char part[100001];
bool visited[100001];

void dfs(int node) {
	if (visited[node]) return;
	visited[node] = true;
	int partnode = part[node];
	for (int i = 0; i < g[node].size() && isbi; i++) {
//	cout << "TEST@" << endl;
		int next = g[node][i];
//		cout << next << " from " << node << endl;
		if (part[next] == partnode) {
			ofstream fout("bipartite.out");
			fout << ("NO");
			fout.close();
			exit(0);
		}
		if (part[next] == 0) {
			if (partnode == 1) part[next] = 2;
			else part[next] = 1;
			dfs(next);
		}
	}

}

int main() {
	ifstream fin("bipartite.in");
	ios_base::sync_with_stdio(0);
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b;
		g[a - 1].push_back(b - 1);
		g[b - 1].push_back(a - 1);
	}
	for (int j = 0; j < n && isbi; j++) {
		if (part[j] != 0) continue;
		part[j] = 1; // 1, 2
//		cout << "TEST" << endl;
		dfs(j);
	}
	ofstream fout("bipartite.out");
	fout << ("YES");
	fout.close();
}
