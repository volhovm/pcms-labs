#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <deque>
using namespace std;

int n, m;
vector<int> g[100000];
bool visited[100000];
int components[100000];
int number = 0;

void bfs(int start_v) {
	deque<int> deq;
	visited[start_v] = true;
	int v = start_v;
	deq.push_front(v);
	while (!deq.empty()) {
		int node = deq.back();
//		visited[node] = true;
//		cout << "DB " << node << " " << number << endl;
		deq.pop_back();
		components[node] = number;
		for (int i = 0; i < g[node].size(); i++) {
			int child = g[node][i];
			if (!visited[child]) {
				deq.push_front(child);
				visited[child] = true;
			}
		}
	}
}

int main() {
	ifstream fin("components.in");
	ofstream fout;
	fout.open("components.out");
	fin >> n >> m;
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b;
		g[a - 1].push_back(b - 1);
		g[b - 1].push_back(a - 1);
	}
	for (int i = 0; i < n; i++) {
		if (!visited[i]) {
			number++;
			bfs(i);
		}
	}
	fout << number << endl;
	for (int i = 0; i < n; i++) {
		fout << components[i] << " ";
	}
	fout.close();
}
