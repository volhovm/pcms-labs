#include <vector>
#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <set>
using namespace std;

struct Node {
    int id;
    bool isChoice;
};
struct ChoiceNode : Node {
    int pred;
    int l, r;
    ChoiceNode(int p, int a, int b):
        pred(p), l(a), r(b) {
        isChoice = true;
    }
};

struct Leaf : Node {
    int value;
    Leaf(int v): value(v) {
        isChoice = false;
    }
};

int simplifyTree(vector<Node*>& tree,
                 int nodeindex,
                 unordered_map<int,bool>& pred_res) {
    Node* node = tree[nodeindex];

    if (!node->isChoice) {
        return nodeindex;
    }

    ChoiceNode* curr = (ChoiceNode*) node;

    // lift subtree
    if (pred_res.count(curr->pred) != 0) {
        if (pred_res[curr->pred] == true) {
            return simplifyTree(tree, curr->r, pred_res);
        } else {
            return simplifyTree(tree, curr->l, pred_res);
        }
    }

    pred_res[curr->pred] = false;
    int indL = simplifyTree(tree, curr->l, pred_res);
    pred_res[curr->pred] = true;
    int indR = simplifyTree(tree, curr->r, pred_res);
    pred_res.erase(curr->pred);

    curr->l = indL;
    curr->r = indR;
    return nodeindex;
}

int newtreecount = 0;

int rehangTree(vector<Node*>& oldtree,
               vector<Node*>& newtree,
               int from, int to) {
    Node* node = oldtree[from];
    newtree[to] = node;
    newtreecount++;

    if (!node->isChoice) {
        return to;
    } else {
        ChoiceNode* curr = (ChoiceNode*) node;
        int newl = rehangTree(oldtree, newtree, curr->l, to+1);
        int newr = rehangTree(oldtree, newtree, curr->r, newl+1);
        curr->l = to+1;
        curr->r = newl+1;
        return newr;
    }
}

int main() {
        ios::sync_with_stdio(0);
        ifstream fin("trees.in");
        ofstream fout;
        fout.open("trees.out");

        int n;
        fin >> n;

        vector<Node*> tree(n);
        for (int i = 0; i < n; i++) {
            string var;
            fin >> var;
            Node n;
            if (var == "choice") {
                int a, b, c;
                fin >> a >> b >> c;
                tree[i] = new ChoiceNode(a, b-1, c-1);
            } else {
                int v;
                fin >> v;
                tree[i] = new Leaf(v);
            }
        }

        unordered_map<int, bool> preds;
        simplifyTree(tree, 0, preds);

        vector<Node*> newtree(n);
        rehangTree(tree, newtree, 0, 0);

        fout << newtreecount << endl;
        for (int i = 0; i < newtreecount; i++) {
            Node* nd = newtree[i];
            if (nd->isChoice) {
                ChoiceNode* curr = (ChoiceNode*) nd;
                fout << "choice " << curr->pred <<
                    " " << curr->l + 1 <<
                    " " << curr->r + 1 << endl;
            } else {
                Leaf* curr = (Leaf*) nd;
                fout << "leaf " << curr->value << endl;
            }
        }

        fout.close();
}
