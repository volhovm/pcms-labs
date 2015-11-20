#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<int> g[25000];
vector<int> h[25000];
vector<int> d;
vector<int> out;
bool visited[25000];
bool visited2[25000];
int num = 0;

void dfs1(int v) {
	visited[v] = true;
	for (int i = 0; i < g[v].size(); i++) {
		int cr = g[v][i];
		if (!visited[cr]) dfs1(cr);
	}
	d.push_back(v);	
}

void dfs2(int v) {
	visited2[v] = true;
	for (int i = 0; i < h[v].size(); i++) {
		int cr = h[v][i];
		if (!visited2[cr]) dfs2(cr);
	}
//	cout << "pushing " << v << endl;
	out[v] = num;
}

void topsort() {
	for (int i = 0; i < n; i++) {
		if (!visited[i]) dfs1(i);
	}
	reverse(d.begin(), d.end());
}

void exec() { 
	for (int i = 0; i < d.size(); i++) {
		int vrt = d[i];
		if (!visited2[vrt]) {
			num++;
			dfs2(vrt);
		}
	}
}

int main() {
	ifstream fin("cond.in");
	ofstream fout;
	fout.open("cond.out");
	fin >> n >> m;
	out = vector<int>(n);
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b;
		g[a - 1].push_back(b - 1);
		h[b - 1].push_back(a - 1);
	}
	topsort();
	for (size_t i = 0; i < d.size(); i++) cout << d[i] + 1 << " " << endl;
	exec();
	fout << num << endl;
    for (size_t i = 0; i < out.size(); i++) fout << out[i] << " ";
}
