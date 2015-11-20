#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <limits>
using namespace std;

int n, m, s, t;
vector<pair<int, int>> g[100555];
bool visited[100555];
vector<int> out;
int st, en;
long long d[100555];

void dfs(int v) {
	visited[v] = true;
	for (int i = 0; i < g[v].size(); i++) {
		int nx = g[v][i].first;
		if (!visited[nx]) dfs(nx);
	}
	out.push_back(v);
}

void top_sort() {
	out.clear();
//	for (long long i = 0; i < n; i++) {
//		if (!visited[i]) dfs(i);
//	}
	dfs(s);
	reverse(out.begin(), out.end());
	st = 0;
	en = 0;
	for (size_t i = 0; i < out.size(); i++) {
		if (out[i] == s) {
			st = i;
			break;
		}
	}
	for (size_t i = 0; i < out.size(); i++) {
		if (out[i] == t) {
			en = i;
			break;
		}
	}
}

void shortpath(){
//	cout << endl << "st = " + to_string(st) + ("; en = ") + to_string(en) << endl;
	for (int i = 0; i < 100000; i++) {
		d[i] = numeric_limits<int>::max() - 20000;
	}
	d[s] = 0;
//	for (long long i = 0; i <= n; i++) {
//		cout << to_string(i) + " " + to_string(d[i]) << endl;
//	}
	for (int i = st; i <= en; i++) {
	    int vvv = out[i];
//		cout << "I = " + to_string(vvv) << endl;
		for (size_t j = 0; j < g[vvv].size(); j++) {
//			cout << "TEST" << endl;
			int v = g[vvv][j].first;
			int w = g[vvv][j].second;
//			cout << to_string(v) + " " + to_string(w) + " " + to_string(d[vvv]) + " " + to_string(d[v]) << endl;
			d[v] = min(d[v], (d[vvv] + w)); 
		}
	}
}

int  main() {
//	cout << to_string(min(-10, 2147483647)) << endl;
	ifstream in("shortpath.in");
	ofstream fout;
	fout.open("shortpath.out");
	in >> n >> m >> s >> t;
	t--;
	s--;
	if (s == t) {
		fout << "0";
		fout.close();
		return 0;
	}
	out = vector<int>(n);
	for (int i = 0; i < m; i++) {
		int a, b, w;
		in >> a >> b >> w;
		g[a - 1].push_back(pair<int, int>(b - 1, w));
//		ginv[b - 1].push_back(pair<long long, long long>(a - 1, w);
	}
//	for (long long i = 0; i < n; i++) {
//		for (long long j = 0; j < g[i].size(); j++) {
//			cout << to_string(i) + " " + to_string(g[i][j].first) + ": " + to_string(g[i][j].second) << endl;
//		}		
//	}
	top_sort();
//	cout << endl;
//	for (long long i = 0; i < out.size(); i++) cout << out[i] << endl;
	if (st >= en) fout << "Unreachable";
	else {		   
		shortpath();
//		for (long long i = 0; i < (en - st); i++) {
//			cout << to_string(d[i]) + " ";
//		}
//		cout << endl;
		fout << d[t];
	}
	fout.close();
}
