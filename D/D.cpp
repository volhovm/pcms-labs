//http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.5306&rep=rep1&type=pdf
#include <stdlib.h>
#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

struct Point {
    long long int x, y;
    Point (long long int a, long long int b) : x(a), y(b)
    {}
    Point operator-(Point &b){
        return Point(x - b.x, y - b.y);
    };
    bool operator<(Point const b) const {
        return y < b.y ? true : x < b.x ? true : false;
    }
    bool operator==(Point const b) const {
        return x == b.x && y == b.y;
    }
};

struct Node {
    Point value;
    int id;
    Node *next, *prev;

    Node(Point v, int i) : value(v), id(i), next(NULL), prev(NULL)
    {}
};

inline int turn(Point a, Point b) {
    long long int turnvalue = ((1ll * a.x * b.y) -
                               (1ll * a.y * b.x));
    return turnvalue > 0 ? 1 : turnvalue < 0 ? -1 : 0;
}

// turn of (a b) (c d)
inline int turn(Point a, Point b, Point c, Point d) {
    return turn(b - a, d - c);
}

bool isConvex(Node *node) {
    return turn(node->prev->value,
                node->value,
                node->value,
                node->next->value) > 0;
}

bool isConcave(Node *node) {
        return turn(node->prev->value,
                node->value,
                node->value,
                node->next->value) < 0;
}

bool isAnEar(Node *node, set<Point> *set) {
    if (turn(node->prev->value, node->value,
             node->value, node->next->value) == 0) return false;
    if (set->empty()) return true;
    else if (isConvex(node)) {
        for (auto i = set->begin(); i != set->end(); i++) {
            Point a = node->prev->value,
                b = node->value,
                c = node->next->value;
            if (*i == a || *i == c) continue;
            //if (*i == b) return false;
            if (turn(b, a, b, *i) < 0 &&
                turn(b, *i, b, c) < 0 &&
                turn(a, c, a, *i) <= 0) return false;
        }
        return true;
    } else return false;
}

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("input.txt");
        ofstream fout;
        fout.open("output.txt");
        int n;
        fin >> n;
        int x0, y0;
        fin >> x0 >> y0;
        Node *n0 = new Node(Point(x0, y0), 1);
        Node *end = n0;
        for (int i = 1; i < n; i++) {
            int x, y;
            fin >> x >> y;
            Node *currnode = new Node(Point(x, y), i + 1);
            currnode->prev = end;
            end->next = currnode;
            end = currnode;
        }
        n0->prev = end;
        end->next = n0;

        Node* currNode = n0;
        // fill set of concave vertices
        set<Point>* R = new set<Point>();
        for (int i = 0; i < n; i++, currNode = currNode->next) {
            if (isConcave(currNode)) {
                R->insert(currNode->value);
            }
        }

        currNode = n0->next->next;
        while (currNode != n0) {
            if (isAnEar(currNode->prev, R) &&
                currNode->next->next->next != currNode) {

                fout << currNode->prev->prev->id << " "
                     << currNode->prev->id << " "
                     << currNode->id << endl;

                Node *prevNode = currNode->prev;
                prevNode->prev->next = currNode;
                currNode->prev = prevNode->prev;

                if (R->count(currNode->value) > 0 && isConvex(currNode)) {
                    R->erase(currNode->value);
                }
                    // lol that's funny
                    // this essential algorithm part should be ignored for the problem to solve
                    // if (R->count(currNode->prev->value) > 0 && isConvex(currNode->prev)) {
                    //     R->erase(currNode->prev->value);
                    // }
                if (currNode->prev == n0) {
                    currNode = currNode->next;
                }
            } else {
                currNode = currNode->next;
            }
        }

        // left triangle
        fout << currNode->id << " "
             << currNode->next->id << " "
             << currNode->prev->id << endl;

        fout.close();
}
