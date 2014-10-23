#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
using namespace std;

int n, s, f;
int g[20001][20001];
int d[20001];
bool used[20001];

class CompareDist
{
public:
    bool operator()(pair<int,int> n1,pair<int,int> n2)
		{
			if(n1.first>n2.first)
				return true;
			else
				return false;

		}
};

void djkstra(int v) {
	for (int i = 0; i < n; i++) d[i] = 2147483646;
	d[v] = 0;
	int curr;
	while (true) {
		int min = 2147483646;
		int minindex = -1;
		for (int i = 0; i < n; i++) {
			if (!used[i] && d[i] < min) {
				min = d[i];
				minindex = i;
			}
		}
		if (minindex == -1) break;
		curr = minindex;
		cout << "in v#" << curr << endl;
		used[curr] = true;
		for (int i = 0; i < n; i++) {
			if (g[curr][i] != -1 && i != curr && !used[i]) {
	  	  	    if (d[i] > d[curr] + g[curr][i]) {
					d[i] = d[curr] + g[curr][i];					
					cout << "- from " << curr << " to " << i << "    " << d[curr] <<	endl;
				}
			}
		}
	}		
}

int main() {
	ios::sync_with_stdio(0);
    ifstream fin("pathmgep.in");
	ofstream fout;
	fout.open("pathmgep.out");
	fin >> n >> s >> f;
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++) {
			fin >> g[i][j];
		}
	djkstra(s - 1);
	if (d[f - 1] == 2147483646)
		fout << -1;
	else fout << d[f - 1];
	fout.close();
}
