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
    long double x, y;

    point(long double a, long double b) : x(a),y(b) {}

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

    point operator/(long double k){
        return point(x/k, y/k);
    }

    bool operator<(point const b) const {
        if (x < b.x) return true;
        if (x > b.x) return false;
        if (y < b.y) return true;
        return false;
    }

    long double norm() { return sqrt(x*x + y*y); }

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

long double distance(point a, point b) { return (b - a).norm(); }

struct segment {
    point start, end;
    segment (point a, point b) : start(a), end(b) {};
    point middle() {
        return point((start.x+end.x)/2, (start.y+end.y)/2);
    }
};

struct circle{
public:
    point center;
    long double radius;

    circle(point a, long double b) : center(a), radius(b) {}

    bool inside(point p) {
        return distance(p, center) <= radius;
    }

    friend ostream& operator<<(ostream& os, const circle& p);
};

ostream& operator<<(ostream& os, const circle& p) {
    os << "[" << p.center << " " << p.radius << "]";
    return os;
};

// http://www.abecedarical.com/zenosamples/zs_circle3pts.html
circle from_3_points(point a, point b, point c) {
    long double M11 = a.x*(b.y-c.y)+c.x*(a.y-b.y)+b.x*(c.y-a.y);
    long double sum0 = a.x*a.x+a.y*a.y;
    long double sum1 = b.x*b.x+b.y*b.y;
    long double sum2 = c.x*c.x+c.y*c.y;
    long double M12 = sum0*b.y+sum2*a.y+sum1*c.y-sum2*b.y-sum1*a.y-sum0*c.y;
    long double M13 = sum0*b.x+sum2*a.x+sum1*c.x-sum2*b.x-sum1*a.x-sum0*c.x;
    long double x0 =   0.5 * M12 / M11;
    long double y0 = - 0.5 * M13 / M11;
    point center = point(x0, y0);
    long double radius = distance(center, a);
    return circle(center, radius);
};

circle min_disc2(vector<point>& points, int pos0, int pos1) {
    point p0 = points[pos0];
    point p1 = points[pos1];
    circle circ(segment(p0,p1).middle(),
                distance(p0,p1)/2);
    cerr << "min_disc2 init: " << circ << endl;
    for (int i = 0; i < min(pos1,pos0); i++) {
        if (!circ.inside(points[i])) {
            circ = from_3_points(points[i], p0, p1);
        }
    }
    return circ;
    cerr << "min_disc2 ret: " << circ << endl;
}

circle min_disc1(vector<point>& points, int pos) {
    point p = points[pos];
    circle circ(segment(points[0],p).middle(),
                distance(points[0],p)/2);
    for (int i = 1; i < pos; i++) {
        if (!circ.inside(points[i])) {
            circ = min_disc2(points, pos, i);
        }
    }
    return circ;
}

circle min_disc0(vector<point>& points) {
    random_shuffle(points.begin(), points.end());
    if (points.size() == 1) return circle(points[0], 0);
    circle circ(segment(points[0],points[1]).middle(),
                distance(points[0], points[1])/2);
    for (int i = 2; i < points.size(); i++) {
        if (!circ.inside(points[i])) {
            circ = min_disc1(points, i);
        }
    }
    return circ;
}

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("tower.in");
        ofstream fout;
        fout.open("tower.out");

        int n;
        fin >> n;
        vector<point> points;

        for (int i = 0; i < n; i++) {
            long double a, b;
            fin >> a >> b;
            points.emplace_back(a,b);
        }

        circle ans = min_disc0(points);

        fout << fixed << setprecision(16)
             << ans.radius << endl
             << ans.center.x << " " << ans.center.y << endl;

        cerr << fixed << setprecision(16)
             << ans.radius << endl
             << ans.center.x << " " << ans.center.y << endl;


        fout.close();
}
