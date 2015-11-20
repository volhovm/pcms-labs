#include <string>
#include <algorithm>
#include <fstream>
#include <vector>
#include <deque>
#include <iostream>
using namespace std;

int n, m;
vector<int> g[30001];
int d[30001];

int main() {
	ios::sync_with_stdio(0);
	ifstream fin("pathbge1.in");
	ofstream fout;
	fout.open("pathbge1.out");
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
	    fin >> a >> b;
	    g[a - 1].push_back(b - 1);
	    g[b - 1].push_back(a - 1);
	}
	deque<int> deq;
	int curr;
	deq.push_back(0);
	for (int i = 0; i < 30001; i++) d[i] = -1;
	d[0] = 0;
	while (!deq.empty()){
		curr = deq.front();
		deq.pop_front();
		for (int i = 0; i < g[curr].size(); i++) {
			int to = g[curr][i];
			if (d[to] == -1) {
				deq.push_back(to);
				d[to] = d[curr] + 1;
			}
		}
	}
	for (int i = 0; i < n; i++) {
		fout << d[i] << " ";
	}
	fout.close();
}
