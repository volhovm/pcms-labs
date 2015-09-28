#include <vector>
#include <cassert>

using namespace std;

vector < double > genTest(int testId) {
    if (testId == 1) {
        vector < double > data {
            0, 2,    2, 1,
                1, 0,    2, 2,

                1, 3,    3, 3,
                2, 3,    2, 4,

                3, 0,    4, 0,
                3, 2,    4, 1
                };
        return data;
    } else if (testId == 2) {
        vector<double> data {
            1, 1, 3, 2,
                0, 0.5, 6, 3.5
        };
        return data;
    }
}
