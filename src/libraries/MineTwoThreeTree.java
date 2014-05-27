package libraries;

/**
 * Created by volhovm on 01.04.14.
 */
@Deprecated
public class MineTwoThreeTree {
    int treeSize = 0;
    Node head;

    public void insert(int x) {
        if (treeSize == 0) {
            head = new Leaf(x, null);
        } else if (treeSize == 1) {
            int firstLeafValue = ((Leaf) head).value > x ? x : ((Leaf) head).value;
            int secondLeafValue = ((Leaf) head).value < x ? x : ((Leaf) head).value;
            head = new TwoNode(
                secondLeafValue,
                new Leaf(firstLeafValue, null),
                new Leaf(secondLeafValue, null),
                null
            );
            ((Leaf) ((TwoNode) head).left).parent = head;
            ((Leaf) ((TwoNode) head).middle).parent = head;
        } else {
            Node current = head;
            while (current != null) {
                if (current instanceof TwoNode) {
                    if (((TwoNode) current).middleMin > x) {
                        current = ((TwoNode) current).left;
                    } else {
                        current = ((TwoNode) current).middle;
                    }
                } else if (current instanceof ThreeNode) {
                    if (((ThreeNode) current).middleMin > x) {
                        current = ((ThreeNode) current).left;
                    } else if (((ThreeNode) current).rightMin > x) {
                        current = ((ThreeNode) current).middle;
                    } else {
                        current = ((ThreeNode) current).right;
                    }
                } else if (current instanceof Leaf) {
                    current = ((Leaf) current).parent;
                    if (current instanceof TwoNode){
//                        if (x < ((Leaf) ((TwoNode) current).left).value){

//                        }else if (x < ((Leaf) current.))
                    } else {
                        //pizdez
                    }
                    break;
                }
            }
        }
        treeSize++;
    }

    public void delete() {

    }

    public boolean exists() {
        return false; //FIXME
    }

    public String next(int x) {
        Leaf n = nextNode(x);
        return (n == null) ? "none" : String.valueOf(n.value);
    }

    private Leaf nextNode(int i) {
        return null; //FIXME
    }

    public String prev(int x) {
        Leaf n = nextNode(x);
        return (n == null) ? "none" : String.valueOf(n.value);
    }

    private Leaf prevNode(int i) {
        return null; //FIXME
    }

    class Node {
    }

    class Leaf extends Node {
        int value;
        Node parent;

        Leaf(int value, Node parent) {
            this.value = value;
            this.parent = parent;
        }
    }

    class TwoNode extends Node {
        int middleMin;
        Node left, middle, parent;

        TwoNode(int middleValue,
                Node left, Node middle,
                Node parent) {
            this.middleMin = middleValue;
            this.left = left;
            this.middle = middle;
            this.parent = parent;
        }
    }

    class ThreeNode extends Node {
        int middleMin, rightMin;
        Node left, middle, right, parent;

        private ThreeNode(int middleMin, int rightMin, Node left, Node middle,
                          Node right, Node parent) {
            this.middleMin = middleMin;
            this.rightMin = rightMin;
            this.left = left;
            this.middle = middle;
            this.right = right;
            this.parent = parent;
        }
    }
}
