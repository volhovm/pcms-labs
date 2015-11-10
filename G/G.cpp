#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
using namespace std;

// OK

// Shamos-Hoey algorithm from here:
// http://geomalgorithms.com/a09-_intersect-3.html

struct point {
    long long int x, y;
    point (int a, int b) : x(a), y(b) {};

    point operator-(point &b){
        return point(x - b.x, y - b.y);
    }

    bool operator<(point const b) const {
        if (x < b.x) return true;
        if (x > b.x) return false;
        if (y < b.y) return true;
        return false;
    }
    bool operator==(point const b) const {
        return x == b.x && y == b.y;
    }
    friend ostream& operator<<(ostream& os, const point& p);
};

struct segment {
    point start, end;
    int id;
    segment (point a, point b) : start(a), end(b) {};
    bool operator==(segment const b) const {
        return id == b.id;
    }
};

struct event {
    bool isstart;
    segment* seg;
    event (bool a, segment *s) : isstart(a), seg(s) {};
    event () {};

    bool operator<(event const b) const {
        point p1 = isstart ? seg->start : seg->end;
        point p2 = b.isstart ? b.seg->start : b.seg->end;
        if (p1 == p2) return isstart && !b.isstart;
        return p1 < p2;
    }
    friend ostream& operator<<(ostream& os, const event& ev);
};

ostream& operator<<(ostream& os, const point& p) {
    os << "<" << p.x << " " << p.y << ">";
    return os;
}

ostream& operator<<(ostream& os, const event& ev) {
    os << (ev.isstart ? "start: " : "end: ") << ev.seg->start
       << " " << ev.seg->end;
    return os;
}

inline int turn(point a, point b) {
    long long int turnvalue = ((1ll * a.x * b.y) -
                               (1ll * a.y * b.x));
    return turnvalue > 0 ? 1 : turnvalue < 0 ? -1 : 0;
}

inline int turn(point a, point b, point c, point d) {
    return turn(b - a, d - c);
}

inline int turn(point a, point b, point c) {
    return turn (a, b, a, c);
}

bool intersects(segment &a, segment &b) {
    if (turn(a.start, a.end, b.start) *
        turn(a.start, a.end, b.end) <= 0 &&
        turn(b.start, b.end, a.start) *
        turn(b.start, b.end, a.end) <= 0) return true;
    return false;
}

struct segmentset_compare {
    bool operator() (const segment& lhs, const segment& rhs) const{
        if (lhs.id == rhs.id) return false;
        if (lhs.start < rhs.start) {
            int turn_ = turn(lhs.start,
                             lhs.end,
                             rhs.start);
            if (turn_ > 0) return true;
            return false;
        } else {
            int turn_ = turn(rhs.start,
                             rhs.end,
                             lhs.start);
            if (turn_ > 0) return false;
            return true;
        }
    }
};

int main() {
        ifstream fin("segments.in");
        ofstream fout;
        fout.open("segments.out");

        int n;
        fin >> n;
        if (n == 1) {
            fout << "NO" << endl;
            return 0;
        }

        vector<event> events;
        for (int i = 0; i < n; i++) {
            int x1, y1, x2, y2;
            fin >> x1 >> y1 >> x2 >> y2;
            point a(x1, y1);
            point b(x2, y2);

            segment *s = (a < b) ? (new segment(a, b)) : (new segment(b, a));
            s->id = i + 1;
            events.push_back(event(true, s));
            events.push_back(event(false, s));
            //cerr << " added events: " << events[2*i] << " , " <<
            //    events[2*i+1] << endl;
        }

        sort(events.begin(), events.end());

        //for (int i = 0;i < events.size(); i++) cerr << events[i] << endl;

        set<segment, segmentset_compare> segmentset;
        for (int i = 0; i < events.size(); i++) {
            event curev = events[i];
            segment seg = *curev.seg;
            if (curev.isstart) {
                // Left endpoint
                set<segment, segmentset_compare>::iterator iter =
                    segmentset.upper_bound(seg);
                if (iter == segmentset.begin()) {
                    if (segmentset.size() > 0) {
                        segment segB = *iter;
                        if (intersects(seg, segB)) {
                            fout << "YES" << endl;
                            fout << seg.id << " " << segB.id << endl;
                            fout.close();

                            cerr << "YES" << endl;
                            cerr << seg.id << " " << segB.id << endl;
                            return 0;
                        }
                    }
                } else if (iter == segmentset.end()) {
                    if (segmentset.size() > 0) {
                        iter--;
                        segment segB = *iter;
                        if (intersects(seg, segB)) {
                            fout << "YES" << endl;
                            fout << seg.id << " " << segB.id << endl;
                            fout.close();

                            cerr << "YES" << endl;
                            cerr << seg.id << " " << segB.id << endl;
                            return 0;
                        }
                    }
                } else {
                    segment segA = *iter;
                    iter--;
                    segment segB = *iter;
                    if (intersects(seg, segB)) {
                            fout << "YES" << endl;
                            fout << seg.id << " " << segB.id << endl;
                            fout.close();

                            cerr << "YES" << endl;
                            cerr << seg.id << " " << segB.id << endl;
                            return 0;
                    }
                    if (intersects(seg, segA)) {
                            fout << "YES" << endl;
                            fout << seg.id << " " << segA.id << endl;
                            fout.close();

                            cerr << "YES" << endl;
                            cerr << seg.id << " " << segA.id << endl;
                            return 0;
                    }
                }
                segmentset.insert(seg);
            } else {
                // Right endpoint
                segmentset.erase(seg);
                set<segment, segmentset_compare>::iterator iter =
                    segmentset.upper_bound(seg);
                if (segmentset.size() <= 1) {
                    // skip
                } if (iter != segmentset.end() && iter != segmentset.begin()) {
                    segment segA = *iter;
                    iter--;
                    segment segB = *iter;
                    if (intersects(segA, segB)) {
                            fout << "YES" << endl;
                            fout << segA.id << " " << segB.id << endl;
                            fout.close();

                            cerr << "YES" << endl;
                            cerr << segA.id << " " << segB.id << endl;
                            return 0;
                    }
                }
            }
        }

        fout << "NO" << endl;
        cerr << "NO" << endl;
        fout.close();
}
