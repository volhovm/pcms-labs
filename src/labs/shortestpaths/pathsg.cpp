#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
using namespace std;

int n, m;
int g[200][200];
int d[200][200];

int main() {
	ios::sync_with_stdio(0);
    ifstream fin("pathsg.in");
	ofstream fout;
	fout.open("pathsg.out");
	fin >> n >> m;
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			if (i == j) g[i][j] = 0; else g[i][j] = 21474836;
	for (int i = 0; i < m; i++) {
		int a, b, w;
		fin >> a >> b >> w;
		g[a - 1][b - 1] = w;
	}
	for (int k = 0; k < n; k++) {
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				g[i][j] = min(g[i][j], g[i][k] + g[k][j]);	
			}
		}
	}
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
		    if (g[i][j] == 2147483646)
					fout << -1 << " ";
			else fout << g[i][j] << " ";
		}
		fout << endl;
	}
	fout.close();
}
