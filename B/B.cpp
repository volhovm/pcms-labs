#include <cstddef>
#include "tests.h"
#include <cmath>
#include <limits>
#include <iomanip>
#include <iostream>
#include <algorithm>
#include <gmpxx.h>
#include <iostream>
#include <vector>

//#define DEBUG

using namespace std;

struct Point {
    Point(double a, double b) : x(a), y(b) {};
    long double x, y;
};


int turn(Point p1, Point a1,
          Point p2, Point b1) {
    long double eps = std::numeric_limits<long double>::epsilon();
    long double a1x = a1.x - p1.x;
    long double a1y = a1.y - p1.y;
    long double b1x = b1.x - p2.x;
    long double b1y = b1.y - p2.y;

    #ifdef DEBUG
    cout << "a1x, a1y, b1x, b1y:" << a1x << " "
         << a1y << " "
         << b1x << " "
         << b1y << endl;
    #endif

    long double inaccuracy = ((long double) 8) * eps * fabs(a1x * a1y + b1x * b1y);
    long double result = a1x * b1y - a1y * b1x;

    #ifdef DEBUG
    cout << "result " << result << endl;
    #endif

    if (abs(result) > inaccuracy) {
        if (result > 0) return 1;
        if (result < 0) return -1;
        return result;
    } else {
    #ifdef DEBUG
        cout << "Using rational" << endl;
    #endif
        mpq_class a1xm((double)a1.x);
        mpq_class a1ym((double)a1.y);
        mpq_class p1xm((double)p1.x);
        mpq_class p1ym((double)p1.y);
        mpq_class b1xm((double)b1.x);
        mpq_class b1ym((double)b1.y);
        mpq_class p2xm((double)p2.x);
        mpq_class p2ym((double)p2.y);

        mpq_class a1dx(a1xm-p1xm);
        mpq_class a1dy(a1ym-p1ym);
        mpq_class b1dx(b1xm-p2xm);
        mpq_class b1dy(b1ym-p2ym);

        mpq_class result(a1dx*b1dy - a1dy*b1dx);
        mpq_class zero(0);

        int res = cmp(result, zero);

        #ifdef DEBUG
        cout << "res: " << res << endl;
        #endif

        if (res > 0) return 1;
        if (res < 0) return -1;
        return 0;
    }
}

bool intersects(Point a1, Point a2,
                Point b1, Point b2) {
    if (a1.x > b1.x && a1.x > b2.x && a2.x > b1.x && a2.x > b2.x) return false; // A1 is X-greater
    if (a1.x < b1.x && a1.x < b2.x && a2.x < b1.x && a2.x < b2.x) return false; // B1 is X-greater
    if (a1.y > b1.y && a1.y > b2.y && a2.y > b1.y && a2.y > b2.y) return false; // A1 is Y-greater
    if (a1.y < b1.y && a1.y < b2.y && a2.y < b1.y && a2.y < b2.y) return false; // B1 is Y-greater
    int turn1 = turn(a1, a2, a1, b1);
    int turn2 = turn(a1, a2, a1, b2);
    #ifdef DEBUG
    cout << "turn 1, 2: " << turn1 << " " << turn2 << endl;
    #endif
    if (abs(turn1 + turn2) == 2) return false;
    int turn3 = turn(b1, b2, b1, a1);
    int turn4 = turn(b1, b2, b1, a2);
    #ifdef DEBUG
    cout << "turn 3, 4: " << turn3 << " " << turn4 << endl;
    #endif
    if (abs(turn3 + turn4) == 2) return false;
    return true;
}

int main() {
    int id;
    cin >> id;
    vector<double> input = genTest(id);
    for (int i = 0; i < input.size(); i += 8) {
    #ifdef DEBUG
        cout << endl;
        cout << "---------------------" << endl;
    #endif
        if (intersects(Point(input[i], input[i+1]),
                       Point(input[i+2], input[i+3]),
                       Point(input[i+4], input[i+5]),
                       Point(input[i+6], input[i+7]))) {
            cout << "Y";
        } else {
            cout << "N";
        }
    }
    cout << endl;
}
