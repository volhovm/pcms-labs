#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctgmath>
using namespace std;

int get_matches(string& vec) {
    cout << vec << endl;
    cout.flush();
    int ret;
    cin >> ret;
    return ret;
}

int main() {
        ios::sync_with_stdio(0);
        int n;
        cin >> n;

        string vec(n, '0');
        int start = get_matches(vec);
        for (int i = 0; i < n; i++) {
            vec[i] = '1';
            int cur = get_matches(vec);
            if (cur == n) break;
            if (cur < start) vec[i] = '0';
            else start++;
        }
}
