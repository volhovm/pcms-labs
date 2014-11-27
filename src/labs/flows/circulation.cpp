#include <string.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <climits>
#include <vector>
#include <queue>
#include <set>
using namespace std;

struct edge {
    int from, to, low, cap, flow, num;
};

const int INF = INT_MAX - 1;
const int MAXN = 205;
int n;
int s = 0;
int t;
int dist[MAXN], q[MAXN], ptr[MAXN];
vector<edge> edges;

//indexes of edges in 'edges' outcoming from g[i]
vector<int> g[MAXN];

void add_edge(int from, int to, int low, int cap, int num) {
    edge e1 = {from, to, low, cap, 0, num};
    edge e2 = {to, from, low, 0, 0, -1};
    g[from].push_back((int) edges.size());
    g[to].push_back((int) edges.size() + 1);
    edges.push_back(e1);
    edges.push_back(e2);
}

bool bfs() {
    int c1 = 0;
    int c2 = 0;
    q[c2++] = s;
    memset(dist, -1, n * sizeof dist[0]);
    dist[s] = 0;
    while (c1 < c2 && dist[t] == -1) {
	int v = q[c1++];
	for (size_t i = 0; i < g[v].size(); i++) {
	    int curr = g[v][i];
	    int to = edges[curr].to;
	    if (dist[to] == -1 && edges[curr].flow < edges[curr].cap) {
		q[c2++] = to;
		dist[to] = dist[v] + 1;
	    }
	}
    }
    return dist[t] != -1;
}

int dfs(int v, int flow) {
    if (!flow) return 0;
    if (v == t) return flow;
    for (; ptr[v] < (int) g[v].size(); ptr[v]++) {
	int curr = g[v][ptr[v]];
	int to = edges[curr].to;
	if (dist[to] != dist[v] + 1) continue;
	int pushed = dfs(to, min(flow, edges[curr].cap - edges[curr].flow));
	if (pushed) {
	    edges[curr].flow += pushed;
	    // if curr % 2 = 0 then curr + 1 else curr - 1
	    edges[curr^1].flow -= pushed;
	    return pushed;
	}
    }
    return 0;
}

int main() {
	ios::sync_with_stdio(0);
	ifstream fin("circulation.in");
	ofstream fout;
	fout.open("circulation.out");
	fin >> n;
	n += 2;
	t = n - 1;
	int m;
	fin >> m;
	for (int i = 0; i < m; i++) {
	    int a, b, l, c;
	    fin >> a >> b >> l >> c;
	    add_edge(a, b, l, c - l, i + 1);
	    add_edge(s, b, 0, l, -1);
	    add_edge(a, t, 0, l, -1);
	}

	int flow = 0;
	while (true) {
	    if (!bfs()) break;
	    memset(ptr, 0, n * sizeof ptr[0]);
	    int pushed;
	    while (true) {
		pushed = dfs(s, INF);
		if (pushed == 0) break;
		flow += pushed;
	    }
	}

	bool circ = true;
	for (int i = 0; i < g[s].size(); i++) {
	    edge e = edges[g[s][i]];
	    if (e.cap != e.flow) {
		circ = false;
		break;
	    }
	}

	if (!circ) fout << "NO";
	else {
	    fout << "YES" << endl;
	    for (int i = 0; i < edges.size(); i++) {
		edge e = edges[i];
		if (e.num > 0) fout << e.flow + e.low << endl;
	    }
	}
	fout.close();
}
