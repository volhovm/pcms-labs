#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

int n, m;
vector<pair<int, int> > g[30001];
int d[30001];
bool used[30001];

void djkstra(int v) {
	for (int i = 0; i < n; i++) d[i] = 2147483646;
	d[v] = 0;
	int curr;
	set< pair<int, int> > queue;
	queue.insert(make_pair(d[v], v));
//	cout << "TEST" << endl;
	while (!queue.empty()) {
		curr = (*queue.begin()).second;
		queue.erase(queue.begin());
//		cout << "in v#" << curr << endl;
		used[curr] = true;
		for (int i = 0; i < g[curr].size(); i++) {
			int to = g[curr][i].first;
			if (!used[to]) {
	  	  	    if (d[to] > d[curr] + g[curr][i].second) {
					queue.erase(make_pair(d[to], to));
					d[to] = d[curr] + g[curr][i].second;
					queue.insert(make_pair(d[to], to));
					//				cout << "- from " << curr << " to " << i << "    " << d[curr] <<	endl;
				}
			}
		}
	}		
}

int main() {
	ios::sync_with_stdio(0);
    ifstream fin("pathbgep.in");
	ofstream fout;
	fout.open("pathbgep.out");
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b, w;
		fin >> a >> b >> w;
		g[a - 1].push_back(pair<int, int>(b - 1, w));
		g[b - 1].push_back(pair<int, int>(a - 1, w));
	}
	djkstra(0);
	for (int i = 0; i < n; i++) {
		if (d[i] == 2147483646)
			fout << -1 << " ";
		else fout << d[i] << " ";
	}
	fout.close();
}
