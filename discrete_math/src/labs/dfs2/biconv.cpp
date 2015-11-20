#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>
using namespace std;

int n, m;
vector<pair<int, int> > g[20005];
bool used[20005];
int in[20005], out[20005];
vector<int> stack;
vector<int> comps;
int compsnumber = 0;
int timer;

void dfs(int v, int parent) {
	timer++;
	in[v] = timer;
	out[v] = timer;
	used[v] = true;
	for (int i = 0; i < g[v].size(); i++) {
		int u = g[v][i].first;
		int uE = g[v][i].second;
//		cout << v << " " << to << endl;
		if (u == parent) {
			continue;
		}
		if (!used[u]) {
			stack.push_back(uE);
			dfs(u, v);
			if (out[u] >= in[v]) {
				compsnumber++;
				while (uE != stack.back()) {
//					cout << "(" << v << ", " << u << ")" << stack.back() <<  endl;
					comps[stack.back()] = compsnumber;
				    stack.pop_back();
				}
				comps[stack.back()] = compsnumber;
				stack.pop_back();
			}
			if (out[u] < out[v]) out[v] = out[u];
		} else {
			if (in[u] < in[v]) stack.push_back(uE);
			if (out[v] > in[u]) out[v] = out[u];
		}	
    }
}

void find_comps() {
	for (int i = 0; i < n; i++) {
		if (!used[i]) {
			timer = 0;
			dfs(i, -1);
		}
	}
}

// TL 9, read with scanf 
int main() {
	ifstream fin("biconv.in");
	ofstream fout;
	fout.open("biconv.out");
	fin >> n >> m;
	comps.resize(m);	
	for (int i = 0; i < m; i++) {
		int a, b;
		fin >> a >> b; 
		g[a - 1].push_back(pair<int, int>(b - 1, i));
		g[b - 1].push_back(pair<int, int>(a - 1, i));
	}
	find_comps();
	fout << compsnumber << endl;
	for (int i = 0; i < m; i++) fout << comps[i] << " ";
	fout.close();
}
