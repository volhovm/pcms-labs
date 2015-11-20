package libraries;

import java.io.PrintWriter;

/**
 * Created by volhovm on 3/20/14.
 */
public class DSU {
    DSUNode[] nodes;
    public DSU(int n) {
        nodes = new DSUNode[n];
        for (int i = 0; i < n; i++) {
            nodes[i] = new DSUNode(i, i, 1, -1);
        }
    }

    public void printAllNodeData(int index, PrintWriter printWriter) {
        DSUNode classRepresentative = nodes[get(index)];
        printWriter.println(classRepresentative.min + " " + classRepresentative.max + " " + classRepresentative.count);
    }

    public int get(int index) {
        if (nodes[index].parentIndex == -1) {
            return index;
        } else {
            int current = get(nodes[index].parentIndex);
            nodes[index].parentIndex = current;
            return current;
        }
    }

    public void union(int a, int b) {
        int aFatherIndex = get(a);
        int bFatherIndex = get(b);
        if (aFatherIndex == bFatherIndex) return;
        DSUNode aFather = nodes[aFatherIndex];
        DSUNode bFather = nodes[bFatherIndex];
        if (aFather.count > bFather.count) {
            bFather.parentIndex = aFatherIndex;
            aFather.min = StrictMath.min(aFather.min, bFather.min);
            aFather.max = StrictMath.max(aFather.max, bFather.max);
            aFather.count += bFather.count;
        } else {
            aFather.parentIndex = bFatherIndex;
            bFather.min = StrictMath.min(aFather.min, bFather.min);
            bFather.max = StrictMath.max(aFather.max, bFather.max);
            bFather.count += aFather.count;
        }
    }

    private class DSUNode {
        int min, max, count, parentIndex;

        private DSUNode(int min, int max, int count, int parentIndex) {
            this.min = min;
            this.max = max;
            this.count = count;
            this.parentIndex = parentIndex;
        }
    }
}
