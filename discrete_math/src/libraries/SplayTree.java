package libraries;

/**
 * @author volhovm
 *         Created on 08.04.14
 */

@Deprecated
public class SplayTree {
    private Node root;

    private Node rightRotation(Node h) {
        Node x = h.leftNode;
        h.leftNode = x.rightNode;
        x.rightNode = h;
        return x;
    }

    private Node leftRotation(Node h) {
        Node x = h.rightNode;
        h.rightNode = x.leftNode;
        x.leftNode = h;
        return x;
    }

    private int findKey(Node node) { //doesn't work
        if (node == null) {
            return 0;
        }
        if (node.leftNode == null) {
            return 0;
        } else {
            return size(node.leftNode);
        }
    }

    private int size(Node node) {
        if (node == null) {
            return 0;
        }
        if (node.leftNode == null && node.rightNode == null) {
            return 1;
        } else if (node.leftNode != null && node.rightNode == null) {
            return 1 + size(node.leftNode);
        } else if (node.leftNode == null) {
            return 1 + size(node.rightNode);
        } else {
            return 1 + size(node.leftNode) + size(node.rightNode);
        }
    }

    private Node max(Node node) {
        if (node == null) {
            return null;
        }
        if (node.rightNode == null) {
            return node;
        }
        return max(node.rightNode);
    }

    public String listAll() {
        return listAll(root);
    }

    public String listAll(Node node) {
        return ((node.leftNode == null ? "" : listAll(node.leftNode))
             + node.value + " "
            + (node.rightNode == null ? "" : listAll(node.rightNode)));
    }

    private Node splay(Node h, int key) {
        if (key == findKey(h)) {
            return h;
        } else if (key > findKey(h)) {
            if (h.rightNode == null) {
                return h;
            }

            if (h.rightNode.leftNode != null && key - findKey(h) - 1 < findKey(h.rightNode)) {
                h.rightNode.leftNode = splay(h.rightNode.leftNode, key);
                if (h.rightNode.leftNode != null) {
                    h.rightNode = rightRotation(h.rightNode);
                }
            } else if (h.rightNode.rightNode != null && key - findKey(h) - 1 > findKey(h.rightNode)) {
                h.rightNode.rightNode = splay(h.rightNode.rightNode, key);
                h = leftRotation(h);
            }
            if (h.rightNode == null) {
                return h;
            } else {
                return leftRotation(h);
            }
        } else {
            if (h.leftNode == null) {
                return h;
            }

            if (h.leftNode.leftNode != null && key < findKey(h.leftNode)) {
                Node ttt = splay(h.leftNode.leftNode, key);
                h.leftNode.leftNode = ttt;
                h = rightRotation(h);
            } else if (h.leftNode.rightNode != null && key > findKey(h.leftNode)) {
                h.leftNode.rightNode = splay(h.leftNode.rightNode, key);
                if (h.leftNode.rightNode != null) {
                    h.leftNode = leftRotation(h.leftNode);
                }
            }
            if (h.leftNode == null) {
                return h;
            } else {
                return rightRotation(h);
            }
        }
    }

    //a < b
    public void reconstruct(int a, int b) {
        NodePair y = split(root, a - 2);
        Node l = y.a;
        Node m = y.b;
        NodePair x = split(m, b - a );
        Node r = x.b;
        m = x.a;
        root = merge(m, merge(l, r));
    }

    public NodePair split(Node node, int key) {
        if (node == root) {
            root = splay(root, key);
            Node l = new Node(root.value);
            l.leftNode = root.leftNode;
            Node r = null;
            if (root.rightNode != null) {
                r = new Node(root.rightNode.value);
                r.leftNode = root.rightNode.leftNode;
                r.rightNode = root.rightNode.rightNode;
            }
            l.rightNode = null;
            return new NodePair(l, r);
        } else {
            root.rightNode = splay(node, key);
            Node l = new Node(root.rightNode.value);
            l.leftNode = root.rightNode.leftNode;
            Node r = null;
            if (root.rightNode.rightNode != null) {
                r = new Node(root.rightNode.rightNode.value);
                r.leftNode = root.rightNode.rightNode.leftNode;
                r.rightNode = root.rightNode.rightNode.rightNode;
            }
            l.rightNode = null;
            return new NodePair(l, r);
        }
    }

    public Node merge(Node a, Node b) {
        a = splay(a, size(a));
        a.rightNode = b;
        return a;
    }

    public void add(int value, int key) {
        if (root == null) {
            root = new Node(value);
            return;
        }
        NodePair x = split(root, key);
        root = new Node(value);
        root.rightNode = x.b;
        root.leftNode = x.a;
        //        root = splay(root, key);
        //        if (key > findKey(root)) {
        //            Node n = new Node(value);
        //            n.leftNode = root.leftNode;
        //            n.rightNode = root;
        //            root.leftNode = null;
        //            root = n;
        //        } else if (key < findKey(root)) {
        //            Node n = new Node(value);
        //            n.rightNode = root.rightNode;
        //            n.leftNode = root;
        //            root.rightNode = null;
        //            root = n;
        //        } else {
        //            root.value = value;
        //        }
    }

    public boolean exists(int key) {
        return (find(key) != -1661661);
    }

    public int find(int key) {
        root = splay(root, key);
        if (key == findKey(root.leftNode)) {
            return root.value;
        } else {
            return -1661661;
        }
    }

    //    public int height() { return height(root); }
    //
    //    private int height(Node x) {
    //        if (x == null) {
    //            return -1;
    //        }
    //        return 1 + Math.max(height(x.leftNode), height(x.rightNode));
    //    }

    //
    //    public int size() {
    //        return size(root);
    //    }
    //
    //    private int size(Node x) {
    //        if (x == null) {
    //            return 0;
    //        } else {
    //            return (1 + size(x.leftNode) + size(x.rightNode));
    //        }
    //    }

    private class Node {
        private int value;
        private Node leftNode, rightNode;

        public Node(int value) {
            this.value = value;
        }
    }

    class NodePair {
        Node a, b;

        NodePair(Node a, Node b) {
            this.a = a;
            this.b = b;
        }
    }
}



