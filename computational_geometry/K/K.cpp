#include <vector>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <iomanip>
#include <set>
using namespace std;

// OK

struct point {
public:
    long long int x, y;

    point(long long int a, long long int b) : x(a),y(b) {}

    point() : x(228),y(228) {}

    point operator+(point &b){
        return point(x + b.x, y + b.y);
    }

    point operator-(point &b){
        return point(x - b.x, y - b.y);
    }

    point operator-(){
        return point(-x, -y);
    }

    bool operator<(point const b) const {
        if (x < b.x) return true;
        if (x > b.x) return false;
        if (y < b.y) return true;
        return false;
    }

    long long int halfnorm() { return x*x + y*y; }

    bool operator==(point const b) const {
        return x == b.x && y == b.y;
    }
    bool operator!=(point const b) const {
        return x != b.x || y != b.y;
    }

    friend ostream& operator<<(ostream& os, const point& p);
};

ostream& operator<<(ostream& os, const point& p) {
    os << "<" << p.x << " " << p.y << ">";
    return os;
}

inline long long int turn(point a, point b) {
    return (1ll * a.x * b.y) - (1ll * a.y * b.x);
}

inline long long int turn(point a, point b, point c, point d) {
    return turn(b - a, d - c);
}

inline long long int turn(point a, point b, point c) {
    return turn (a, b, a, c);
}

long long int halfdist(point a, point b) { return (b - a).halfnorm(); }

void graham(vector<point>& points) {
    if (points.size() <= 2) {
        return;
    }
    auto minpoint = min_element(points.begin(), points.end());
    point minimum_point = *minpoint;
    swap(*minpoint, points[0]);
    sort(points.begin()+1, points.end(),
         [&minimum_point] (point a, point b) {
                 long long int t = turn(minimum_point, a, b);
                 if (t == 0) {
                     return halfdist(a, minimum_point) <
                            halfdist(b, minimum_point);
                 }
                 else return t > 0;
         });

    int k = 1;
    for (int i = 2; i < points.size();) {
        while (k > 0 && turn(points[k-1], points[k], points[i]) <= 0) k--;
        swap(points[k+1], points[i]);
        k++;
        i++;
    }
    points.resize(k+1);
}

inline int sign(long long int a) { return a > 0 ? 1 : (a < 0 ? -1 : 0); }

struct segment {
    point start, end;
    segment (point a, point b) : start(a), end(b) {};
    segment (): start(point(0,0)), end(point(0,0)) {};
    friend ostream& operator<<(ostream& os, const segment& p);
};

ostream& operator<<(ostream& os, const segment& p) {
    os << "[" << p.start << " " << p.end << "]";
    return os;
};

struct line {
    long long int a, b, c;
    line(long long int x, long long int y, long long int z) : a(x), b(y), c(z) {}
    bool intersects_with(segment& seg) {
        long long int val0 = 1ll*a*seg.start.x + 1ll*b*seg.start.y + c;
        long long int val1 = 1ll*a*seg.end.x + 1ll*b*seg.end.y + c;
        if (val0 == 0 || val1 == 0) return true;
        return sign(val0) != sign(val1);
    }
};

long long int dist_line_point(line l, point p) {
    return abs(l.a * p.x + l.b * p.y + l.c);
}

bool intersection(line l, vector<point>& points) {
    if (points.empty()) return false;
    if (points.size() == 1) {
        point p = points[0];
        return l.a * p.x + l.b * p.y + l.c == 0;
    }
    if (points.size() == 2) {
        segment seg(points[0], points[1]);
        return l.intersects_with(seg);
    }

    int step;
    for (step = 1; (step+1)*2 < points.size(); step *= 2) {}

    int startindex = 0;
    int index0 = (startindex)        % points.size();
    int index1 = (startindex+step)   % points.size();
    int index2 = (startindex+2*step) % points.size();
    segment seg0 = segment(points[index0], points[index1]);
    segment seg1 = segment(points[index1], points[index2]);
    segment seg2 = segment(points[index0], points[index2]);
    if (l.intersects_with(seg0) ||
        l.intersects_with(seg1) ||
        l.intersects_with(seg2))
        return true;

    long long int dist0 = dist_line_point(l, points[index0]);
    long long int dist1 = dist_line_point(l, points[index1]);
    long long int dist2 = dist_line_point(l, points[index2]);

    long long int mindist = min(dist0, min(dist1, dist2));
    int searchindex;
    if (mindist == dist0) searchindex = index0;
    else if (mindist == dist1) searchindex = index1;
    else if (mindist == dist2) searchindex = index2;

    for (; step > 0; step /= 2) {
        int ind_prev = (points.size()+searchindex-step)%points.size();
        int ind_next = (points.size()+searchindex+step)%points.size();
        segment seg_prev = segment(points[ind_prev], points[searchindex]);
        segment seg_next = segment(points[searchindex], points[ind_next]);
        if (l.intersects_with(seg_prev) ||
            l.intersects_with(seg_next)) return true;

        long long int dist_prev = dist_line_point(l, points[ind_prev]);
        long long int dist_cur  = dist_line_point(l, points[searchindex]);
        long long int dist_next = dist_line_point(l, points[ind_next]);

        long long int mindist = min(dist_prev, min(dist_cur, dist_next));

        if (mindist == dist_prev) searchindex = ind_prev;
        else if (mindist == dist_next) searchindex = ind_next;
    }

    return false;
}

int main() {
    ios::sync_with_stdio(0);
    ifstream fin("highways.in");
    ofstream fout;
    fout.open("highways.out");

    int n, m;
    fin >> n >> m;

    vector<line> lines;
    for (int i = 0; i < n; i++) {
        int a, b, c;
        fin >> a >> b >> c;
        lines.emplace_back(a, b, c);
    }

    vector<point> points;
    for (int i = 0; i < m; i++) {
        int a, b;
        fin >> a >> b;
        points.emplace_back(a, b);
    }

    graham(points);

    vector<int> ans;
    for (int i = 0 ; i < n; i++) {
        if (intersection(lines[i], points))
            ans.push_back(i+1);
    }

    cerr << ans.size() << endl;
    fout << ans.size() << endl;
    for (int i = 0; i < ans.size(); i++) {
        fout << ans[i] << " ";
    }
    fout << endl;
    cerr << endl;

    fout.close();
}
