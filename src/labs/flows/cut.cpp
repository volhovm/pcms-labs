#include <string.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

struct edge {
    int from, to, cap, flow;
};

const int INF = 100000000;
const int MAXN = 500;
int n;
int s = 0;
int t;
int dist[MAXN], q[MAXN], ptr[MAXN];
bool used[MAXN];
vector<edge> edges;
vector<int> ans;

//g[i] = vec of indexes of edges in 'edges' outcoming from i
vector<int> g[MAXN];

void add_edge(int from, int to, int cap) {
    edge e1 = {from, to, cap, 0};
    edge e2 = {to, from, cap, 0};
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


void dfs2(int v){
    if (used[v]) return;
//    std::cout << "in " << v << std::endl;
    ans.push_back(v);
    used[v] = true;
    for (; ptr[v] < (int) g[v].size(); ptr[v]++) {
	int curr = g[v][ptr[v]];
	int to = edges[curr].to;
	if (edges[curr].cap > edges[curr].flow) {
	    dfs2(to);
	}
    }
}

int main() {
	ios::sync_with_stdio(0);
//	ifstream fin("cut.in");
	ifstream fin("cut.in");
	ofstream fout;
	fout.open("cut.out");
	fin >> n;
	t = n - 1;
	int m;
	fin >> m;
	for (int i = 0; i < m; i++) {
	    int a, b, cap;
	    fin >> a >> b >> cap;
	    add_edge(a - 1, b - 1, cap);
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
	memset(ptr, 0, n * sizeof ptr[0]);
	dfs2(s);
	fout << ans.size() << endl;
//	cout << ans.size() << endl;
	for (size_t i = 0; i < ans.size(); i++) {
//	    cout << ans[i] + 1 << " ";
	    fout << ans[i] + 1 << " ";
	}
	fout.close();
}
