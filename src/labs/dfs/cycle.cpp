#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <algorithm>
using namespace std;

int n;
vector<int> g[100000];
int visited[100000]; //0 white, 1 grey, 2 black
bool found = false;
bool written = false;
vector<int> out;

void dfs(int v) {
	visited[v] = 1;
	for (size_t i = 0; i < g[v].size(); i++) {
		int to = g[v][i];
		if (visited[to] == 0) {
			dfs (to);
		} else if (visited[to] == 1) {
			out.push_back(to);
			found = true;
		}
		if (found && !written) {
			out.push_back(v);
			if (out.size() > 1 && v == out[0]) {
				written = true;
			}
			break;
		}
		if (written) break;
	}
	visited[v] = 2;
}

int main() {
	ifstream str("cycle.in");
	ofstream strout;
	strout.open("cycle.out");
	int m;
    str >> n >> m;
	out = vector<int>(n);
	out.clear();
	for (size_t i = 0; i < m; i++) {
		int a, b;
		str >> a >> b;
		g[a - 1].push_back(b - 1);
	}
	for (int i = 0; i < n; i++) {
		visited[i] = 0;
	}
	for (int i = 0; i < n; i++) {
	    if (found) break;
		if (visited[i] == 0) dfs(i);
	}
	if (!found) strout << "NO" << endl;
	else {
		strout << "YES" << endl;
		for (int i = out.size() - 1; i > 0; i--){
			strout << (out[i] + 1) << " ";
		}
	}
	strout.close();
}


	
