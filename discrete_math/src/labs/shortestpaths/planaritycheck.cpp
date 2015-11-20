#include <vector>
#include <fstream>
#include <iostream>
#include <string>
#include <cmath>
using namespace std;

int m, n;
bool g[6][6];

bool isbipart(int parts[]) {
	for(int i = 0; i < 6; ++i) {
		if(parts[i] != 0) continue;
		for(int j = 0; j < 6; ++j) {
			if(parts[j] != 1) continue;
			if(!g[i][j]) return false;
		}
	}
	return true;
}

void delv(int u) {
	for(int k = u; k < 5; k++) {
		for(int i = 0; i < 6; i++) {
			g[k][i] = g[k + 1][i];
			g[i][k] = g[i][k + 1];
		}
	}
}

void contre(int u, int v) {
	for(int i = 0; i < 6; ++i) {
		if(g[u][i]) {
			g[v][i] = true;
			g[i][v] = true;
		}
		g[u][i] = false;
		g[i][u] = false;
	}
	delv(u);
}

bool containsK33() {
	int parts[6];
	for (int i = 0; i < 6; i++) parts[i] = 0;
	for(int i = 0; i < 4; i++) {
		for(int j = i + 1; j < 5; j++) {
			for(int k = j + 1; k < 6; k++) {
				parts[i] = 1;
				parts[j] = parts[i];
				parts[k] = parts[j];
				if(isbipart(parts)) return true;
				for (int i = 0; i < 6; i++) parts[i] = 0;
			}
		}
	}
	return false;
}

bool isK5() {
	for (int i = 1; i < 5; i++) 
		for (int j = 0; j < i; j++) {
//			cout << "ISK5" << endl;
			if (!g[i][j]) return false;
		}
//	cout << "k5!!!" << endl;
	return true;
}

bool containsK5() {
	bool copy[6][6];
	for(int i = 0; i < 6; i++) 
		for (int j = 0; j < 6; j++)
			copy[i][j] = g[i][j];
	for (int v = 0; v < 6; v++) {
		delv(v);
		if(isK5()) return true;
		for (int i = 0; i < 6; i++)
			for (int j = 0; j < 6; j++) 
				g[i][j] = copy[i][j];
	}
	for(int u = 1; u < 6; u++) {
		for(int v = 0; v < u; v++) {
			if(g[u][v]) {
				contre(u, v);
				if (isK5()) return true;
				for (int i = 0; i < 6; i++)
					for (int j = 0; j < 6; j++) 
						g[i][j] = copy[i][j];
			}
		}
	}
	for (int i = 0; i < 6; i++)
		for (int j = 0; j < 6; j++) 
			g[i][j] = copy[i][j];
	return false;
}

void dump() {
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			cout << g[i][j] << " ";
		}
		cout << endl;
	}
}

bool nonPlanar() {
//  dump();
	if (n < 5) return false;
//  cout << "TEST" << endl;
	if (n == 5) return isK5();
//	cout << "TEST" << endl;
	return containsK5() || containsK33();
}

int main()
{
	std::ios::sync_with_stdio(0);
	ifstream fin("planaritycheck.in");
	ofstream fout;
	fout.open("planaritycheck.out");
	fin >> m; 
	for(int i = 0; i < m; i++) {
		string str;
		fin >> str;
		n = (int) ceil(sqrt(str.length() * 2));
//		cout << n << " str is: " <<  str << endl;
		for(int k = 1; k < n; k++) {
			for(int j = 0; j < k; j++) {
				g[k][j] = str.at(j + k * (k - 1) / 2) == '1';
				g[j][k] = g[k][j];
			}
		}
		if (nonPlanar()) {
		    fout << "NO" << endl;
		} else {
			fout << "YES" << endl;
		}
	}
}
