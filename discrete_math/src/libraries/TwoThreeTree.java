package libraries;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Iterator;


public class TwoThreeTree {

    static class Node {
        private Node parent;
        private Node leftChild;
        private Node rightChild;
        private Node middleChild;

        // When node is 2-node, leftVal is the values, and rightVal is null.
        private int leftVal;
        private int rightVal;

        private boolean twoNode;


        protected Node() {

        }

        public static Node newTwoNode(int value) {
            Node node = new Node();
            node.leftVal = value;
            node.twoNode = true;
            return node;
        }


        public static Node newThreeNode(int leftVal, int rightVal) {
            Node node = new Node();
            if (leftVal > rightVal) {
                node.rightVal = leftVal;
                node.leftVal = rightVal;
            } else {
                node.leftVal = leftVal;
                node.rightVal = rightVal;
            }
            node.twoNode = false;
            return node;
        }


        public static HoleNode newHole() {
            return new HoleNode();
        }


        public void setLeftChild(Node leftChild) {
            this.leftChild = leftChild;
            if (leftChild != null) {
                leftChild.setParent(this);
            }
        }

        public void removeChildren() {
            this.leftChild = null;
            this.rightChild = null;
        }


        public void setRightChild(Node rightChild) {
            this.rightChild = rightChild;
            if (rightChild != null) {
                rightChild.setParent(this);
            }
        }

        public void setMiddleChild(Node middleChild) {
            assert isThreeNode();
            this.middleChild = middleChild;
            if (middleChild != null) {
                middleChild.setParent(this);
            }
        }


        public final Node parent() {
            return parent;
        }

        public final void setParent(Node parent) {
            this.parent = parent;
        }


        public boolean isTerminal() {
            return leftChild == null && rightChild == null;
        }


        public int val() {
            assert isTwoNode();
            return leftVal;
        }


        public int leftVal() {
            assert isThreeNode();
            return leftVal;
        }

        public void setVal(int val) {
            assert isTwoNode();
            leftVal = val;
        }


        public int rightVal() {
            assert isThreeNode();
            return rightVal;
        }

        public void setLeftVal(int leftVal) {
            assert isThreeNode();
            this.leftVal = leftVal;
        }

        public void setRightVal(int rightVal) {
            assert isThreeNode();
            this.rightVal = rightVal;
        }

        public boolean isTwoNode() {
            // return rightVal == null;
            return twoNode;
        }

        public boolean isThreeNode() {
            return !isTwoNode();
        }

        public Node leftChild() {
            return leftChild;
        }

        public Node rightChild() {
            return rightChild;
        }

        public Node middleChild() {
            assert isThreeNode();
            return middleChild;
        }


        public void replaceChild(Node currentChild, Node newChild) {
            if (currentChild == leftChild) {
                leftChild = newChild;
            } else if (currentChild == rightChild) {
                rightChild = newChild;
            } else {
                assert middleChild == currentChild;
                middleChild = newChild;
            }
            newChild.setParent(this);
            currentChild.setParent(null);
        }
    }
    static final private class HoleNode extends Node {
        private Node child;

        HoleNode() {
            super();
        }

        public boolean isTwoNode() {
            return false;
        }

        public Node sibling() {
            if (parent() != null) {
                return parent().leftChild() == this ? parent().rightChild() : parent().leftChild();
            }
            return null;
        }

        @Override
        public void setLeftChild(Node leftChild) {
        }

        @Override
        public void removeChildren() {
            child = null;
        }


        @Override
        public void setRightChild(Node rightChild) {
        }

        public Node child() {
            return child;
        }

        public void setChild(Node child) {
            this.child = child;
        }
    }


    Node root;
    int size = 0;

    public boolean add(Integer value) {
        if (root == null) {
            root = Node.newTwoNode(value);
        } else {
            try {
                Node result = insert(value, root);
                if (result != null) {
                    root = result;
                }
            } catch (DuplicateException e) {
                return false;
            }
        }
        size++;
        return true;
    }

    public boolean contains(Integer value) {
        return findNode(root, value) != null;
    }

    private Node findNode(Node node, Integer value) {
        if (node == null) {
            return null;
        }

        if (node.isThreeNode()) {
            int leftComp = value.compareTo(node.leftVal());
            int rightComp = value.compareTo(node.rightVal());
            if (leftComp == 0 || rightComp == 0) {
                return node;
            }
            if (leftComp < 0) {
                return findNode(node.leftChild(), value);
            } else if (rightComp < 0) {
                return findNode(node.middleChild(), value);
            } else {
                return findNode(node.rightChild(), value);
            }
        } else {
            int comp = value.compareTo(node.val());
            if (comp == 0) {
                return node;
            }
            if (comp < 0) {
                return findNode(node.leftChild(), value);
            } else {
                return findNode(node.rightChild(), value);
            }
        }
    }

    //    public String next(Integer i) {
    //        //lol lost my brain flushed somewhere
    //        Integer last = last();
    //        if (i.compareTo(last) == 1) return "none";
    //        else {
    //            for (int j = i + 1; j <= last; j++) {
    //                if (contains(j)) return String.valueOf(j);
    //            }
    //        }
    //        return "none";
    //    }
    //
    //    public String prev(Integer i) {
    //        int first = first();
    //        if (i.compareTo(first) == -1) return "none";
    //        else {
    //            for (int j = i - 1; j >= first; j--) {
    //                if (contains(j)) return String.valueOf(j);
    //            }
    //        }
    //        return "none";
    //    }
    //
    public String next(int x) {
        int n = nextNode(x);
        return (n == Integer.MAX_VALUE) ? "none" : String.valueOf(n);
    }

    private int nextNode(int x) {
        if (root == null) {
            return Integer.MAX_VALUE;
        } else if (size == 1) {
            if (x >= last()) {
                return Integer.MAX_VALUE;
            } else {
                return root.leftVal();
            }
        } else if (size == 2) {
            if (x >= last()) {
                return Integer.MAX_VALUE;
            } else {
                if (root.leftVal() > x) {
                    return root.leftVal();
                } else {
                    return root.rightVal();
                }
            }
        } else {
            Node currentNode = root;
            int nextValue = Integer.MAX_VALUE;
            while (!currentNode.isTerminal()) {
                if (currentNode.isTwoNode()) {
                    if (x < currentNode.leftVal()) {
                        nextValue = currentNode.leftVal();
                        currentNode = currentNode.leftChild();
                    } else {
                        currentNode = currentNode.rightChild();
                    }
                } else if (currentNode.isThreeNode()) {
                    if (x < currentNode.leftVal()) {
                        nextValue = currentNode.leftVal();
                        currentNode = currentNode.leftChild();
                    } else if (x < currentNode.rightVal()) {
                        nextValue = currentNode.rightVal();
                        currentNode = currentNode.middleChild();
                    } else {
                        currentNode = currentNode.rightChild();
                    }
                }
            }
            if (currentNode.isTwoNode()) {
                if (currentNode.leftVal() > x) {
                    return currentNode.leftVal();
                } else {
                    return nextValue;
                }
            } else {
                if (currentNode.leftVal() > x) {
                    return currentNode.leftVal();
                } else if (currentNode.rightVal() > x) {
                    return currentNode.rightVal();
                } else return nextValue;
            }
        }
    }

    public String prev(int x) {
        int n = prevNode(x);
        return (n == Integer.MIN_VALUE) ? "none" : String.valueOf(n);
    }

    private int prevNode(int x) {
        if (root == null) {
            return Integer.MIN_VALUE;
        } else if (size == 1) {
            if (x <= first()) {
                return Integer.MIN_VALUE;
            } else {
                return root.leftVal();
            }
        } else if (size == 2) {
            if (x <= first()) {
                return Integer.MIN_VALUE;
            } else {
                if (root.rightVal() < x) {
                    return root.rightVal();
                } else {
                    return root.leftVal();
                }
            }
        } else {
            Node currentNode = root;
            int prevValue = Integer.MIN_VALUE;
            while (!currentNode.isTerminal()) {
                if (currentNode.isTwoNode()) {
                    if (x > currentNode.leftVal()) {
                        prevValue = currentNode.leftVal();
                        currentNode = currentNode.rightChild();
                    } else {
                        currentNode = currentNode.leftChild();
                    }
                } else if (currentNode.isThreeNode()) {
                    if (x > currentNode.rightVal()) {
                        prevValue = currentNode.rightVal();
                        currentNode = currentNode.rightChild();
                    } else if (x > currentNode.leftVal()) {
                        prevValue = currentNode.leftVal();
                        currentNode = currentNode.middleChild();
                    } else {
                        currentNode = currentNode.leftChild();
                    }
                }
            }
            if (currentNode.isTwoNode()) {
                if (currentNode.leftVal() < x) {
                    return currentNode.leftVal();
                } else {
                    return prevValue;
                }
            } else {
                if (currentNode.rightVal() < x) {
                    return currentNode.rightVal();
                } else if (currentNode.leftVal() < x) {
                    return currentNode.leftVal();
                } else return prevValue;
            }
        }
    }

    private static final class DuplicateException extends RuntimeException {
    }

    ;
    private static final DuplicateException DUPLICATE = new DuplicateException();

    private Node insert(Integer value, Node node) throws DuplicateException {
        Node returnValue = null;
        if (node.isTwoNode()) {
            int comp = value.compareTo(node.val());

            if (node.isTerminal()) {
                if (comp == 0) {
                    throw DUPLICATE;
                }
                Node thnode = Node.newThreeNode(value, node.val());
                Node parent = node.parent();
                if (parent != null) {
                    parent.replaceChild(node, thnode);
                } else {
                    root = thnode;
                }
            } else {
                if (comp < 0) {
                    Node result = insert(value, node.leftChild());
                    if (result != null) {
                        Node threeNode = Node.newThreeNode(result.val(), node.val());
                        threeNode.setRightChild(node.rightChild());
                        threeNode.setMiddleChild(result.rightChild());
                        threeNode.setLeftChild(result.leftChild());
                        if (node.parent() != null) {
                            node.parent().replaceChild(node, threeNode);
                        } else {
                            root = threeNode;
                        }
                        unlinkNode(node);
                    }
                } else if (comp > 0) {
                    Node result = insert(value, node.rightChild());
                    if (result != null) {
                        Node threeNode = Node.newThreeNode(result.val(), node.val());
                        threeNode.setLeftChild(node.leftChild());
                        threeNode.setMiddleChild(result.leftChild());
                        threeNode.setRightChild(result.rightChild());
                        if (node.parent() != null) {
                            node.parent().replaceChild(node, threeNode);
                        } else {
                            root = threeNode;
                        }
                        unlinkNode(node);
                    }
                } else {
                    throw DUPLICATE;
                }
            }

        } else { // three node
            Node threeNode = node;

            int leftComp = value.compareTo(threeNode.leftVal());
            int rightComp = value.compareTo(threeNode.rightVal());
            if (leftComp == 0 || rightComp == 0) {
                throw DUPLICATE;
            }

            if (threeNode.isTerminal()) {

                returnValue = splitNode(threeNode, value);

            } else {
                if (leftComp < 0) {
                    Node result = insert(value, threeNode.leftChild());
                    if (result != null) {
                        returnValue = splitNode(threeNode, result.val());
                        returnValue.leftChild().setLeftChild(result.leftChild());
                        returnValue.leftChild().setRightChild(result.rightChild());
                        returnValue.rightChild().setLeftChild(threeNode.middleChild());
                        returnValue.rightChild().setRightChild((threeNode.rightChild()));
                        unlinkNode(threeNode);
                    }
                } else if (rightComp < 0) {
                    Node result = insert(value, threeNode.middleChild());
                    if (result != null) {
                        returnValue = splitNode(threeNode, result.val());
                        returnValue.leftChild().setLeftChild(threeNode.leftChild());
                        returnValue.leftChild().setRightChild(result.leftChild());
                        returnValue.rightChild().setLeftChild(result.rightChild());
                        returnValue.rightChild().setRightChild(threeNode.rightChild());
                        unlinkNode(threeNode);
                    }
                } else {
                    Node result = insert(value, threeNode.rightChild());
                    if (result != null) {
                        returnValue = splitNode(threeNode, result.val());
                        returnValue.leftChild().setLeftChild(threeNode.leftChild());
                        returnValue.leftChild().setRightChild(threeNode.middleChild());
                        returnValue.rightChild().setLeftChild(result.leftChild());
                        returnValue.rightChild().setRightChild(result.rightChild());
                        unlinkNode(threeNode);
                    }
                }
            }
        }
        return returnValue;
    }

    public boolean remove(Integer value) {
        if (value == null) {
            return false;
        }
        //  System.out.println("removing " + value);
        Node node = findNode(root, value);
        if (node == null) {
            return false;
        }

        HoleNode hole = null;
        Node terminalNode;
        Integer holeValue;
        if (node.isTerminal()) {
            terminalNode = node;
            holeValue = value;
        } else {
            // Replace by successor.
            if (node.isThreeNode()) {
                if (node.leftVal() == value) {
                    Node pred = predecessor(node, value);
                    holeValue = pred.isThreeNode() ? pred.rightVal() : pred.val();
                    node.setLeftVal(holeValue);
                    terminalNode = pred;
                } else {
                    Node succ = successor(node, value);
                    holeValue = succ.isThreeNode() ? succ.leftVal() : succ.val();
                    node.setRightVal(holeValue);
                    terminalNode = succ;
                }
            } else {
                Node succ = successor(node, value);
                holeValue = succ.isThreeNode() ? succ.leftVal() : succ.val();
                node.setVal(holeValue);
                terminalNode = succ;
            }
        }

        assert terminalNode.isTerminal();

        if (terminalNode.isThreeNode()) {
            // Easy case. Replace 3-node by 2-node
            Integer val = terminalNode.leftVal() == holeValue ? terminalNode.rightVal() :
                          terminalNode.leftVal();
            Node twoNode = Node.newTwoNode(val);
            if (terminalNode.parent() != null) {
                terminalNode.parent().replaceChild(terminalNode, twoNode);
            } else {
                root = twoNode;
            }
        } else {
            if (terminalNode.parent() != null) {
                hole = Node.newHole();
                terminalNode.parent().replaceChild(terminalNode, hole);
            } else {
                root = null;
            }
        }

        // For description of each case see
        // "2-3 Tree Deletion: Upward Phase" in  http://cs.wellesley
        // .edu/~cs230/spring07/2-3-trees.pdf
        while (hole != null) {
            // Case 1. The hole has a 2-node as parent and 2-node as sibling.
            if (hole.parent().isTwoNode() && hole.sibling().isTwoNode()) {
                //System.out.println("Case 1");
                Node parent = hole.parent();
                Node sibling = hole.sibling();

                Node threeNode = Node.newThreeNode(parent.val(), sibling.val());
                if (parent.leftChild() == hole) {
                    threeNode.setLeftChild(hole.child());
                    threeNode.setMiddleChild(sibling.leftChild());
                    threeNode.setRightChild(sibling.rightChild());
                } else {
                    threeNode.setLeftChild(sibling.leftChild());
                    threeNode.setMiddleChild(sibling.rightChild());
                    threeNode.setRightChild(hole.child());
                }

                if (parent.parent() == null) {
                    unlinkNode(hole);
                    root = threeNode;
                    hole = null;
                } else {
                    hole.setChild(threeNode);
                    parent.parent().replaceChild(parent, hole);
                }
                unlinkNode(parent);
                unlinkNode(sibling);

            }
            // Case 2. The hole has a 2-node as parent and 3-node as sibling.
            else if (hole.parent().isTwoNode() && hole.sibling().isThreeNode()) {
                //System.out.println("Case 2 ");
                Node parent = hole.parent();
                Node sibling = hole.sibling();

                if (parent.leftChild() == hole) {
                    Node leftChild = Node.newTwoNode(parent.val());
                    Node rightChild = Node.newTwoNode(sibling.rightVal());
                    parent.setVal(sibling.leftVal());
                    parent.replaceChild(hole, leftChild);
                    parent.replaceChild(sibling, rightChild);
                    leftChild.setLeftChild(hole.child());
                    leftChild.setRightChild(sibling.leftChild());
                    rightChild.setLeftChild(sibling.middleChild());
                    rightChild.setRightChild(sibling.rightChild());
                } else {
                    Node leftChild = Node.newTwoNode(sibling.leftVal());
                    Node rightChild = Node.newTwoNode(parent.val());
                    parent.setVal(sibling.rightVal());
                    parent.replaceChild(sibling, leftChild);
                    parent.replaceChild(hole, rightChild);
                    leftChild.setLeftChild(sibling.leftChild());
                    leftChild.setRightChild(sibling.middleChild());
                    rightChild.setLeftChild(sibling.rightChild());
                    rightChild.setRightChild(hole.child());
                }
                unlinkNode(hole);
                unlinkNode(sibling);
                hole = null;
            }

            // Case 3. The hole has a 3-node as parent and 2-node as sibling.
            else if (hole.parent().isThreeNode()) {
                Node parent = hole.parent();

                // subcase (a), hole is in the middle
                if (parent.middleChild() == hole && parent.leftChild().isTwoNode()) {
                    //System.out.println("Case 3 (a) hole in the middle");
                    Node leftChild = parent.leftChild();
                    Node newParent = Node.newTwoNode(parent.rightVal());
                    Node newLeftChild = Node.newThreeNode(leftChild.val(), parent.leftVal());
                    newParent.setLeftChild(newLeftChild);
                    newParent.setRightChild(parent.rightChild());
                    if (parent != root) {
                        parent.parent().replaceChild(parent, newParent);
                    } else {
                        root = newParent;
                    }

                    newLeftChild.setLeftChild(leftChild.leftChild());
                    newLeftChild.setMiddleChild(leftChild.rightChild());
                    newLeftChild.setRightChild(hole.child());

                    unlinkNode(parent);
                    unlinkNode(leftChild);
                    unlinkNode(hole);
                    hole = null;
                }
                // subcase (b), hole is in the middle
                else if (parent.middleChild() == hole && parent.rightChild().isTwoNode()) {
                    //System.out.println("Case 3(b) hole in the middle");
                    Node rightChild = parent.rightChild();
                    Node newParent = Node.newTwoNode(parent.leftVal());
                    Node newRightChild = Node.newThreeNode(parent.rightVal(), rightChild.val());
                    newParent.setLeftChild(parent.leftChild());
                    newParent.setRightChild(newRightChild);
                    if (parent != root) {
                        parent.parent().replaceChild(parent, newParent);
                    } else {
                        root = newParent;
                    }
                    newRightChild.setLeftChild(hole.child());
                    newRightChild.setMiddleChild(rightChild.leftChild());
                    newRightChild.setRightChild(rightChild.rightChild());
                    unlinkNode(parent);
                    unlinkNode(rightChild);
                    unlinkNode(hole);
                    hole = null;
                } else if (parent.middleChild().isTwoNode()) {
                    Node middleChild = parent.middleChild();

                    // subcase (a). hole is the left child.
                    if (parent.leftChild() == hole) {
                        //System.out.println("Case 3 (a) hole is left child");
                        Node newParent = Node.newTwoNode(parent.rightVal());
                        Node leftChild = Node.newThreeNode(parent.leftVal(), middleChild.val());
                        newParent.setLeftChild(leftChild);
                        newParent.setRightChild(parent.rightChild());
                        if (parent != root) {
                            parent.parent().replaceChild(parent, newParent);
                        } else {
                            root = newParent;
                        }

                        leftChild.setLeftChild(hole.child());
                        leftChild.setMiddleChild(middleChild.leftChild());
                        leftChild.setRightChild(middleChild.rightChild());

                        unlinkNode(parent);
                        unlinkNode(hole);
                        unlinkNode(middleChild);
                        hole = null;
                    }
                    // subcase (a). hole is the right child.
                    else if (parent.rightChild() == hole) {
                        //System.out.println("Case 3 (a) hole is right child");
                        Node newParent = Node.newTwoNode(parent.leftVal());
                        Node rightChild = Node.newThreeNode(middleChild.val(), parent.rightVal());
                        newParent.setRightChild(rightChild);
                        newParent.setLeftChild(parent.leftChild());
                        if (parent != root) {
                            parent.parent().replaceChild(parent, newParent);
                        } else {
                            root = newParent;
                        }

                        rightChild.setLeftChild(middleChild.leftChild());
                        rightChild.setMiddleChild(middleChild.rightChild());
                        rightChild.setRightChild(hole.child());

                        unlinkNode(parent);
                        unlinkNode(hole);
                        unlinkNode(middleChild);
                        hole = null;
                    }
                }

                // Case 4. The hole has a 3-node as parent and 3-node as sibling.

                else if (parent.middleChild().isThreeNode()) {
                    Node middleChild = parent.middleChild();
                    // subcase (a) hole is the left child
                    if (hole == parent.leftChild()) {
                        //System.out.println("Case 4 (a) hole is left child");
                        Node newLeftChild = Node.newTwoNode(parent.leftVal());
                        Node newMiddleChild = Node.newTwoNode(middleChild.rightVal());
                        parent.setLeftVal(middleChild.leftVal());
                        parent.setLeftChild(newLeftChild);
                        parent.setMiddleChild(newMiddleChild);
                        newLeftChild.setLeftChild(hole.child());
                        newLeftChild.setRightChild(middleChild.leftChild());
                        newMiddleChild.setLeftChild(middleChild.middleChild());
                        newMiddleChild.setRightChild(middleChild.rightChild());

                        unlinkNode(hole);
                        unlinkNode(middleChild);
                        hole = null;
                    }
                    // subcase (b) hole is the right child
                    else if (hole == parent.rightChild()) {
                        // System.out.println("Case 4 (b) hole is right child");
                        Node newMiddleChild = Node.newTwoNode(middleChild.leftVal());
                        Node newRightChild = Node.newTwoNode(parent.rightVal());
                        parent.setRightVal(middleChild.rightVal());
                        parent.setMiddleChild(newMiddleChild);
                        parent.setRightChild(newRightChild);
                        newMiddleChild.setLeftChild(middleChild.leftChild());
                        newMiddleChild.setRightChild(middleChild.middleChild());
                        // newMiddleChild.setParent(middleChild.middleChild());
                        newRightChild.setLeftChild(middleChild.rightChild());
                        newRightChild.setRightChild(hole.child());

                        unlinkNode(hole);
                        unlinkNode(middleChild);
                        hole = null;

                    } else if (hole == parent.middleChild() && parent.leftChild().isThreeNode()) {
                        // System.out.println("Case 4 (a) hole is middle child, left is 3-node");
                        Node leftChild = parent.leftChild();
                        Node newLeftChild = Node.newTwoNode(leftChild.leftVal());
                        Node newMiddleChild = Node.newTwoNode(parent.leftVal());
                        parent.setLeftVal(leftChild.rightVal());
                        parent.setLeftChild(newLeftChild);
                        parent.setMiddleChild(newMiddleChild);
                        newLeftChild.setLeftChild(leftChild.leftChild());
                        newLeftChild.setRightChild(leftChild.middleChild());
                        newMiddleChild.setLeftChild(leftChild.rightChild());
                        newMiddleChild.setRightChild(hole.child());

                        unlinkNode(hole);
                        unlinkNode(leftChild);
                        hole = null;
                    } else {
                        assert (hole == parent.middleChild() && parent.rightChild().isThreeNode());
                        // System.out.println("Case 4 (b) hole is middle child, right is 3-node");
                        Node rightChild = parent.rightChild();
                        Node newRightChild = Node.newTwoNode(rightChild.rightVal());
                        Node newMiddleChild = Node.newTwoNode(parent.rightVal());
                        parent.setRightVal(rightChild.leftVal());
                        parent.setMiddleChild(newMiddleChild);
                        parent.setRightChild(newRightChild);
                        newRightChild.setRightChild(rightChild.rightChild());
                        newRightChild.setLeftChild(rightChild.middleChild());
                        newMiddleChild.setRightChild(rightChild.leftChild());
                        newMiddleChild.setLeftChild(hole.child());

                        unlinkNode(hole);
                        unlinkNode(rightChild);
                        hole = null;
                    }
                }

            }
        }

        size--;
        return true;
    }

    private void unlinkNode(Node node) {
        node.removeChildren();
        node.setParent(null);
    }

    private Node successor(Node node, Integer value) {
        if (node == null) {
            return null;
        }

        if (!node.isTerminal()) {
            Node p;
            if (node.isThreeNode() && node.leftVal() == (value)) {
                p = node.middleChild();
            } else {
                p = node.rightChild();
            }
            while (p.leftChild() != null) {
                p = p.leftChild();
            }
            return p;
        } else {
            Node p = node.parent();
            if (p == null) {
                return null;
            }

            Node ch = node;
            while (p != null && ch == p.rightChild()) {
                ch = p;
                p = p.parent();
            }
            return p != null ? p : null;
        }
    }

    private Node predecessor(Node node, Integer value) {
        if (node == null) {
            return null;
        }

        Node p;
        if (!node.isTerminal()) {
            if (node.isThreeNode() && node.rightVal() == (value)) {
                p = node.middleChild();
            } else {
                p = node.leftChild();
            }

            while (p.rightChild() != null) {
                p = p.rightChild();
            }
            return p;
        } else {
            throw new UnsupportedOperationException(
                "Implement predecessor parent is not terminal node");
        }

    }

    private Node splitNode(Node threeNode, Integer value) {
        Integer min;
        Integer max;
        Integer middle;
        if (value.compareTo(threeNode.leftVal()) < 0) {
            min = value;
            middle = threeNode.leftVal();
            max = threeNode.rightVal();
        } else if (value.compareTo(threeNode.rightVal()) < 0) {
            min = threeNode.leftVal();
            middle = value;
            max = threeNode.rightVal();
        } else {
            min = threeNode.leftVal();
            max = value;
            middle = threeNode.rightVal();
        }

        Node parent = Node.newTwoNode(middle);
        parent.setLeftChild(Node.newTwoNode(min));
        parent.setRightChild(Node.newTwoNode(max));
        return parent;
    }

    public interface Function {
        public void apply(Integer t);
    }

    public void preOrder(Node node, Function f) {
        if (node.isThreeNode()) {
            f.apply(node.leftVal());
            f.apply(node.rightVal());
        }
        if (node.isTerminal()) {
            return;
        }


        preOrder(node.leftChild(), f);
        if (node.isThreeNode()) {
            assert node.middleChild() != null;
            preOrder(node.middleChild(), f);
        }
        preOrder(node.rightChild(), f);
    }

    public void inorderSearch(Node node, Function func) {
        if (node == null) {
            return;
        }
        inorderSearch(node.leftChild(), func);
        if (node.isThreeNode()) {
            Node threeNode = node;
            func.apply(threeNode.leftVal());
            inorderSearch(threeNode.middleChild(), func);
            func.apply(threeNode.rightVal());
        } else {
            func.apply(node.val());
        }
        inorderSearch(node.rightChild(), func);
    }

    public Iterator iterator() {

        return new Iterator() {
            Node nextNode;

            // Stack to keep three nodes
            Deque<Node> threeNodes = new ArrayDeque<Node>();
            Integer next;

            {
                Node node = root;
                while (node != null && node.leftChild() != null) {
                    node = node.leftChild();
                }
                nextNode = node;
            }

            public boolean hasNext() {
                return next != null || nextNode != null;
            }

            public Integer next() {
                Integer prev;
                if (next != null) {
                    prev = next;
                    next = null;
                    nextNode = successor(nextNode, prev);
                    return prev;
                }
                if (nextNode.isThreeNode()) {
                    if (nextNode.isTerminal()) {
                        next = nextNode.rightVal();
                        prev = nextNode.leftVal();
                    } else {
                        if (threeNodes.peekFirst() == nextNode) {
                            threeNodes.pollFirst();
                            prev = nextNode.rightVal();
                            nextNode = successor(nextNode, prev);
                        } else {
                            prev = nextNode.leftVal();
                            threeNodes.addFirst(nextNode);
                            nextNode = successor(nextNode, prev);
                        }
                    }
                } else {
                    prev = nextNode.val();
                    nextNode = successor(nextNode, prev);
                }
                return prev;
            }


            public void remove() {
                throw new UnsupportedOperationException();
            }
        };

    }

    public Integer first() {
        Node node = root;
        while (node.leftChild() != null) {
            node = node.leftChild();
        }
        return node.isThreeNode() ? node.leftVal() : node.val();
    }

    public Integer last() {
        Node node = root;
        while (node.rightChild() != null) {
            node = node.rightChild();
        }
        return node.isThreeNode() ? node.rightVal() : node.val();
    }

    public int size() {
        return size;
    }
}





