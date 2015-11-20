#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<int> g[100000];
bool visited[100000];
vector<int> out;

void dfs(int v) {
	for (int i = 0; i < g[v].size(); i++) {
		int nx = g[v][i];
		if (!visited[nx]) dfs(nx);
	}
	visited[v] = true;
	out.push_back(v);
}

void top_sort() {
	out.clear();
	for (int i = 0; i < n; i++) {
		if (visited[i] == 0) dfs(i);
	}
	reverse(out.begin(), out.end());
}

bool ham(){
	for (size_t i = 0; i < out.size() - 1; i++) {
		if(find(g[out[i]].begin(), g[out[i]].end(), out[i + 1]) == g[out[i]].end()) {
//			cout << "failed at " + to_string(i) << endl;
			return false;
		} 
	}
	return true;
}

int main() {
	ifstream in("hamiltonian.in");
	ofstream fout;
	fout.open("hamiltonian.out");
	in >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
		in >> a >> b;
		g[a - 1].push_back(b - 1);
	}
//	for (int i = 0; i < n; i++) {
//		cout << to_string(i) + ":";
//		for (int j = 0; j < g[i].size(); j++) {
//			cout << " " + to_string(g[i][j]);
//		}
//		cout << endl;
//	}
//	if (ham()) out << "YES";
//	else out << "NO";
	top_sort();
//	for (size_t i = 0; i < out.size(); i++) {
//		cout << to_string(out[i] + 1) + " ";
//	}
	cout << endl;
	if (ham()) fout << "YES";
	else fout << "NO";
	fout.close();
}
