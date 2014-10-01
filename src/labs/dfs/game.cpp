#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <algorithm>
using namespace std;

int n, m, s;
vector<int> g[100000];
bool visited[100000]; //0 white, 1 grey, 2 black
bool game_status[100000];

void dfs(int v) {
	visited[v] = true;
	bool bl = false;
//	if (g[v].size() == 0) bl = true;
//	cout << std::to_string(v) + " " + std::to_string(bl) << endl;
	for (size_t i = 0; i < g[v].size(); i++) {
  		int to = g[v][i];
		if (!visited[to]) {
			dfs (to);
		}
		bl = bl || !game_status[to];
	}
	game_status[v] = bl;
}

int main() {
	ifstream str("game.in");
	ofstream strout;
	strout.open("game.out");
    str >> n >> m >> s;
	for (size_t i = 0; i < m; i++) {
		int a, b;
		str >> a >> b;
		g[a - 1].push_back(b - 1);
	}
	for (int i = 0; i < n; i++) {
		visited[i] = false;
		game_status[i] = false;
	}
	dfs(s - 1);
//	for (int i = 0; i < n; i++) {
//		cout << game_status[i] << endl; 
//	}
	if (game_status[s - 1]) strout << "First player wins";
	else strout << "Second player wins";
	strout.close();
}
