#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;


const int INF = 0xFFFFFFFF / 3;

struct edge {
    int next, inv, flow, cap, cost;
    bool is_fake;

    edge(int next, int inv, int cap, int cost, bool is_fake) :
	next(next), inv(inv), flow(0),
	cap(cap), cost(cost), is_fake(is_fake)
    {}
};

struct graph {
    int n, nn;
    vector<vector<edge> > edges;
    vector<int> d, phi, from;
    vector<bool> used;

    graph(int n) :
	n(n),
	nn((n - 2) / 2),
	edges(n),
	d(n),
	phi(n),
	from(n),
	used(n),
	mt(nn),
	used_kuhn(nn)
    {}

    void add_edge(int from, int to, int cap, int cost, bool is_fake) {
	edges[from].push_back(edge(to, edges[to].size(), cap, cost, is_fake));
	edges[to].push_back(edge(from, edges[from].size() - 1, 0, -cost, true));
    }

    void add_edge(int from, int to, int cap, int cost) {
	add_edge(from, to, cap, cost, false);
    }


    bool dijkstra(int t) {
	//тcout << "djkstra" << endl;
	for(;;) {
	    int v0 = -1;
	    for(int v = 0; v < n; v++)
		if(!used[v] &&
		   d[v] < INF &&
		   (v0 < 0 || d[v0] > d[v]))
		    v0 = v;
	    if(v0 < 0) break;
	    //cout << "dijkstra cycle" << endl;
	    used[v0] = true;
	    for(int e = 0; e < edges[v0].size(); e++)
		if(edges[v0][e].cap > edges[v0][e].flow) {
		    int to = edges[v0][e].next;
		    int opt = d[v0] + edges[v0][e].cost + phi[v0] - phi[to];
		    if(!used[to] && d[to] > opt) {
			d[to] = opt;
			from[to] = edges[v0][e].inv;
		    }
		}
	}
	return used[t];
    }

    template <typename T>
    void dump(vector<T> vec) {
	for (int i = 0 ; i < vec.size(); i++) cout << vec[i] << " ";
	cout << endl;
    }

    long long min_cost_of_max_flow(int s, int t) {
	fill(phi.begin(), phi.end(), INF);
	phi[s] = 0;
	//	dump(phi);
	//initial potentials with ford-bellman
	for(int N = 0; N < n; N++)
	    for(int v = 0; v < n; v++)
		if(phi[v] < INF)
		    for(int e = 0; e < edges[v].size(); e++)
			if(edges[v][e].cap > 0 &&
			   phi[edges[v][e].next] > phi[v] + edges[v][e].cost)
			    phi[edges[v][e].next] = phi[v] + edges[v][e].cost;

	//dump(phi);
	int result_flow = 0;
	long long result_cost = 0;

	for(;;) {
	    //cout << "algo cycle" << endl;
	    fill(used.begin(), used.end(), false);
	    fill(d.begin(), d.end(), INF);
	    d[s] = 0;

	    //dijkstra
	    if(!dijkstra(t)) break;
	    // cout << "dijkstra suceeded" << endl;
	    for(int i = 0; i < n; i++)
		phi[i] += used[i] ? d[i] : d[t];

	    int curr_flow = INF;
	    long long curr_cost = 0;
	    for(int i = t; i != s; ) {
		int v = edges[i][from[i]].next;
		int e = edges[i][from[i]].inv;
		curr_flow = min(curr_flow, edges[v][e].cap - edges[v][e].flow);
		curr_cost += 0ll + edges[v][e].cost;
		i = v;
	    }
	    // correcting edges
	    for(int i = t; i != s; ) {
		int v = edges[i][from[i]].next;
		int e = edges[i][from[i]].inv;
		edges[v][e].flow += curr_flow;
		edges[i][from[i]].flow -= curr_flow;
		i = v;
	    }
	    result_flow += curr_flow;
	    result_cost += 1ll * curr_flow * curr_cost;
	}
	return result_cost;
    }
    vector<int> mt;
    vector<char> used_kuhn;

    bool kuhn (int v) {
	if (used_kuhn[v]) return false;
	//	cout << "kuhn in vertex " << v << endl;
	used_kuhn[v] = true;
	for (int i = 0; i < edges[v].size(); i++) {
	    edge e = edges[v][i];
	    if (!e.is_fake && e.flow > 0) {
		int to = e.next - nn;
		//		cout << " to " << to << " " << mt[to] << endl;
		if (mt[to] == -1 || kuhn (mt[to]) ) {
		    //		    cout << "set kuhn: " << v << " " << to << endl;
		    mt[to] = v;
		    return true;
		}
	    }
	}
	return false;
    }

    vector<int> do_kunh(int k) {
	mt.assign (nn, -1);
	for (int v = 0; v < nn; v++) {
	    //		cout << "from " << v << endl;
	    used_kuhn.assign(nn, false);
	    kuhn (v);
	}

	vector<int> rev(nn);
	for (int i = 0; i < nn; i++)
	    if (mt[i] != -1) {
		//		    cout << mt[i] << " ";
		rev[mt[i]] = i;
		//			    cout << "set edge to fake: " << mt[i] << " -> " << q << endl;
		edges[mt[i]][i].is_fake = true;
	    }
	//	    cout << endl << endl;
	return rev;
    }
};


int main() {
	ios::sync_with_stdio(0);
	ifstream fin("multiassignment.in");
	ofstream fout;
	fout.open("multiassignment.out");
	int n, k;
	fin >> n >> k;
		int matrix[n][n];
	for (int i = 0; i < n; i++)
	    for (int j = 0; j < n; j++)
		fin >> matrix[i][j];
	graph g(n * 2 + 2);
	for (int i = 0; i < n; i++) {
	    for (int j = 0; j < n; j++)
		g.add_edge(i, n + j, 1, matrix[i][j]);
	    g.add_edge(2 * n, i, k, 0, true);
	    g.add_edge(n + i, 2 * n + 1, k, 0, true);
	}
	fout << g.min_cost_of_max_flow(n * 2, n * 2 + 1) << endl;
	for (int i = 0; i < k; i++) {
	    //	    cout << "KUHN #" << i << endl;
	    vector<int> res = g.do_kunh(k);
	    for (int j = 0; j < n; j++) fout << res[j] + 1 << " ";
	    fout << endl;
	}
	fout.close();
}
