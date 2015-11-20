#include <vector>
#include <queue>
#include <algorithm>
#include <fstream>
#include <iostream>
using namespace std;


struct edge {
	edge(int x, int y, int w):
		a(x), b(y), cost(w){}
	int a, b, cost;
};

int n, m;
vector<edge> e;
const int INF = 1000000000;

int main() {
	ifstream fin("negcycle.in");
	ofstream fout;
	fout.open("negcycle.out");
	fin >> n;
	vector<int> d (n);
	vector<int> p (n, -1);
	m = 0;
    for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++) {
			int a;
			fin >> a;
			if (a == INF) continue;
			e.push_back(edge(i, j, a));
			m++;
		}	
	int neg;
	for (int i = 0; i < n; i++) {
		neg = -1;
		for (int j = 0; j < m; j++)
			if (d[e[j].b] > d[e[j].a] + e[j].cost) {
				d[e[j].b] = max (-INF, d[e[j].a] + e[j].cost);
				p[e[j].b] = e[j].a;
				neg = e[j].b;
			}
	}

	if (neg == -1) fout << "NO";
	else {
		fout << "YES" << endl;
		vector<int> path;
		int y = neg;
		for (int i = 0; i < n; i++)
			y = p[y];
		for (int cur = y; true; cur=p[cur]) {
			path.push_back(cur);
			if (path.size() > 1 && cur == y)  break;
		}
		reverse (path.begin(), path.end());

		fout << path.size() << endl;
		for (int i = 0; i < path.size(); i++)
			fout << path[i] + 1 << ' ';
	}
	fout.close();
}
