#include <string.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

struct edge {
    long long from, to, cap, flow, num;
};

const long long INF = 10000000000000000;
const int MAXN = 550;
int n;
int s = 0;
int t;
int dist[MAXN], q[MAXN], ptr[MAXN];
bool used[MAXN];
vector<edge> edges;
vector<vector<int> > ans;

//indexes of edges in 'edges' outcoming from g[i]
vector<int> g[MAXN];

void add_edge(int from, int to, long long cap, int num) {
    edge e1 = {from, to, cap, 0, num};
    edge e2 = {to, from, 0, 0, num};
    g[from].push_back((int) edges.size()); edges.push_back(e1);
    g[to].push_back((int) edges.size());
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

long long dfs(int v, long long flow) {
    if (!flow) return 0;
    if (v == t) return flow;
    for (; ptr[v] < (int) g[v].size(); ptr[v]++) {
	long long curr = g[v][ptr[v]];
	long long to = edges[curr].to;
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

long long dfs2(int v, long long minflow, vector<int>& storage) {
    //    cout << "in v: " << v + 1 << endl;
    if ((v == t) | used[v]) return minflow;
    used[v] = true;
    for (int i = 0; i < g[v].size(); i++) {
	int curr = g[v][i];
	int to = edges[curr].to;
	if (to == v) continue;
	//	cout << "viewing edge: " << edges[curr].from  + 1<< " " <<
	//	    edges[curr].to + 1<< " " << edges[curr].cap << " " << edges[curr].num << endl;
	if (edges[curr].flow > 0 && !used[to]) {
	    //    cout << "chose edge: " << edges[curr].from + 1 << " " <<
	    //	edges[curr].to + 1 << " " << edges[curr].cap << " " << edges[curr].num << endl;
	    minflow = min(minflow, edges[curr].flow);
	    storage.push_back(edges[curr].num);
	    minflow = min(minflow, dfs2(to, minflow, storage));
	    edges[curr].flow -= minflow;
	    break;
	}
    }
    return minflow;
}


int main() {
    ios::sync_with_stdio(0);
    ifstream fin("decomposition.in");
    ofstream fout;
    fout.open("decomposition.out");
    fin >> n;
    t = n - 1;
    int m;
    fin >> m;
    for (int i = 0; i < m; i++) {
	int a, b, cap;
	fin >> a >> b >> cap;
	add_edge(a - 1, b - 1, cap, i + 1);
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
    while (true) {
	vector<int> vec;
	memset(used, false, n * sizeof used[0]);
	long long curr = dfs2(s, INF, vec);
	if (curr == INF) break;
	vec.push_back(curr);
	//	cout << "next cycle, got " << curr << endl;
	ans.push_back(vec);
    }
    cout << endl;
    fout << ans.size() << endl;
    for (int i = 0; i < ans.size(); i++) {
	if (ans[i].size() > 1) {
	    fout << ans[i].back() << " " << ans[i].size() - 1 << " ";
	    for (int j = 0; j < ans[i].size() - 1; j++) {
		fout << ans[i][j]<< " ";
	    }
	    fout << endl;
	}
    }
    fout.close();
}
