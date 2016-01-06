#include <vector>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <iomanip>
#include <set>
using namespace std;

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

inline int turn(point a, point b) {
    long double turnvalue = ((a.x * b.y) -
                             (a.y * b.x));
    return turnvalue > 0 ? 1 : turnvalue < 0 ? -1 : 0;
}

inline int turn(point a, point b, point c, point d) {
    return turn(b - a, d - c);
}

inline int turn(point a, point b, point c) {
    return turn (a, b, a, c);
}

// line intersection
point intersection(point a, point b, point c, point d) {
    // if b - a || d - c then fail(((
    {
        point x = b - a;
        point y = d - c;
        if (x/x.norm() != y/y.norm()) { cout << "LOL FAIL" << endl; exit(1); }
    }
    // like this shit a lot
    long double x = (d.y-a.y+(a.x/b.x)-(d.x/c.x)) /
                 ((b.y-a.y)/(b.x-a.x)-(c.y-b.y)/(c.x-d.x));
    long double y = (b.y-a.y)*(x-a.x)/(b.x-a.x)+a.y;
    return point(x,y);
}

struct segment {
    point start, end;
    segment (point a, point b) : start(a), end(b) {};
    point middle() {
        cout << " middle0: " << start << " " << end << endl;
        cout << " middle1: " << start.x/2.0 << " " << end.x/2.0 << endl;
        long double x0 = start.x/2 + end.x/2;
        long double y0 = start.y/2 + end.y/2;
        cout << " middle2: " << x0 << " " << y0 << endl;
        return point(x0, y0);
    }
    // that's a normal that satisfies `turn (end - start) (normal) > 0`
    point normal() {
        point b0 = end - start;
        return point(-b0.y, b0.x);
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
    // this should work, but slow and cumbersome, and has division
    // by zero problems
    // (serpers method)
    //segment seg1 = segment(p1, p2);
    //segment seg2 = segment(p2, p3);
    //point middle1 = seg1.middle();
    //point middle2 = seg2.middle();
    //point normal1 = seg1.normal();
    //point normal2 = seg2.normal();
    //center = intersect(middle1-normal1, middle1+normal1,
    //                   middle2-normal2, middle2+normal2);
    //radius = distance(center,p1);
    long double M11 = a.x*b.y+c.x*a.y+b.x*c.y-a.x*b.y-b.x*a.y-a.x*c.y;
    long double sum0 = a.x*a.x+a.y*a.y;
    long double sum1 = b.x*b.x+b.y*b.y;
    long double sum2 = c.x*c.x+c.y*c.y;
    long double M12 = sum0*b.y+sum2*a.y+sum1*c.y-sum2*b.y-sum2*a.y-sum0*c.y;
    long double M13 = sum0*b.x+sum2*a.x+sum1*c.x-sum2*b.x-sum2*a.x-sum0*c.x;
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
    cout << "min_disc2 init: " << circ << endl;
    for (int i = 0; i < pos1; i++) {
        if (!circ.inside(points[i])) {
            circ = from_3_points(points[i], p0, p1);
        }
    }
    return circ;
    cout << "min_disc2 ret: " << circ << endl;
}

circle min_disc1(vector<point>& points, int pos) {
    point p = points[pos];
    circle circ(segment(points[0],p).middle(),
                distance(points[0],p)/2);
    cout << "min_disc1 init: " << circ << endl;
    for (int i = 1; i < pos; i++) {
        if (!circ.inside(points[i])) {
            circ = min_disc2(points, pos, i);
        }
    }
    return circ;
    cout << "min_disc1 ret: " << circ << endl;
}

circle min_disc0(vector<point>& points) {
    random_shuffle(points.begin(), points.end());
    cout << points[0] << " " << points[1] << endl;
    cout << segment(points[0],points[1]).middle() << endl;
    circle circ(segment(points[0],points[1]).middle(),
                distance(points[0], points[1])/2);
    cout << "min_disc0 init: " << circ << endl;
    for (int i = 2; i < points.size(); i++) {
        if (!circ.inside(points[i])) {
            cout << "min_disc0_changing" << endl;
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

        fout << fixed << setprecision(9)
             << ans.radius << endl
             << ans.center.x << " " << ans.center.y << endl;

        cerr << fixed << setprecision(9)
             << ans.radius << endl
             << ans.center.x << " " << ans.center.y << endl;


        fout.close();
}
