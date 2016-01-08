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
    os << p.x << " " << p.y;
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

int main() {
    int n;
    cin >> n;

    vector<point> points;
    for (int i = 0; i < n; i++) {
        int a, b;
        cin >> a >> b;

        points.emplace_back(a, b);
    }

    graham(points);

    cout << points.size() << endl;
    for (int i = 0; i < points.size(); i++) {
        cout << points[i] << endl;
    }
}
