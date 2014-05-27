package libraries;

/**
 * @author volhovm
 *         Created on 07.04.14
 */

@Deprecated
public class SplayTreeTry {
    private class Node {
        Node leftSon, rightSon, parent;
        int value;
        boolean isRightSon;

        private Node(int value) {
            this.value = value;
        }

        private Node(Node parent, Node leftSon, Node rightSon, int value, boolean isRightSon) {
            this.leftSon = leftSon;
            this.rightSon = rightSon;
            this.parent = parent;
            this.value = value;
            this.isRightSon = isRightSon;
        }
    }

    Node root = null;

    public void insert(int x) {
        if (root == null) {
            root = new Node(null, null, null, x, true);
        } else {
            Node currentNode = root;
            while (true) {
                if (x == currentNode.value) {
                    break;
                } else if (x < currentNode.value) {
                    if (currentNode.leftSon != null) {
                        currentNode = currentNode.leftSon;
                    } else {
                        currentNode.leftSon = new Node(currentNode, null, null, x, false);
                        break;
                    }
                } else if (x > currentNode.value) {
                    if (currentNode.rightSon != null) {
                        currentNode = currentNode.rightSon;
                    } else {
                        currentNode.rightSon = new Node(currentNode, null, null, x, true);
                        break;
                    }
                }
            }
        }
    }

    private Node find(int x) {
        if (root == null) {
            return null;
        } else {
            Node currentNode = root;
            while (true) {
                if (x == currentNode.value) {
                    splay(currentNode, x);
                    return currentNode;
                } else if (x < currentNode.value) {
                    if (currentNode.leftSon != null) {
                        currentNode = currentNode.leftSon;
                    } else {
                        return null;
                    }
                } else if (x > currentNode.value) {
                    if (currentNode.rightSon != null) {
                        currentNode = currentNode.rightSon;
                    } else {
                        return null;
                    }
                }
            }
        }
    }

    private Node splay(Node currentNode, int key) {
        int oddCounter = 0;
        if (currentNode == null) {
            return null;
        }

        if (key < currentNode.value) {
            if (currentNode.leftSon == null) return currentNode;
            if (key < currentNode.leftSon.value){
                //zigzig left-left
            } else if (key > currentNode.leftSon.value) {
                //zigzag left-right
            } else {
                currentNode = root.leftSon;
                Node a = root;
                root = currentNode;
                a.leftSon = root.rightSon;
                root.rightSon = a;
                a.parent = root;
            }
        } else if (key > currentNode.value){
            if (currentNode.rightSon == null) return currentNode;
            if (key > currentNode.rightSon.value){
                //zigzig right-right
            } else if (key < currentNode.rightSon.value) {
                //zigzag right-left
            } else {
                //zig right
            }
        }
        if (currentNode != root && currentNode.parent == root && oddCounter % 2 == 1) {
            //zig
            if (root.leftSon == currentNode) {
                Node a = root;
                root = currentNode;
                a.leftSon = root.rightSon;
                root.rightSon = a;
                a.parent = root;
            } else {
                Node a = root;
                root = currentNode;
                a.rightSon = root.leftSon;
                root.leftSon = a;
                a.parent = root;
            }

        } else if (currentNode.parent.leftSon == currentNode &&
            currentNode.parent.parent.leftSon == currentNode.parent ||
            currentNode.parent.rightSon == currentNode &&
                currentNode.parent.parent.rightSon == currentNode.parent) {
            //zig zig

            if (currentNode.parent.leftSon == currentNode) {
                Node a = currentNode.parent.parent;
                Node b = currentNode.parent;
                if (a.parent.leftSon == a) {
                    a.parent.leftSon = currentNode;
                } else {
                    a.parent.rightSon = currentNode;
                }
                currentNode.parent = a.parent;
                b.leftSon = currentNode.rightSon;
                currentNode.rightSon.parent = b;
                currentNode.rightSon = b;
                b.parent = currentNode;
                a.leftSon = b.rightSon;
                b.rightSon.parent = a;
                b.rightSon = a;
                a.parent = b;
            } else {
                Node a = currentNode.parent.parent;
                Node b = currentNode.parent;
                if (a.parent.leftSon == a) {
                    a.parent.leftSon = currentNode;
                } else {
                    a.parent.rightSon = currentNode;
                }
                currentNode.parent = a.parent;
                b.rightSon = currentNode.leftSon;
                currentNode.leftSon.parent = b;
                currentNode.leftSon = b;
                b.parent = currentNode;
                a.rightSon = b.leftSon;
                b.leftSon.parent = a;
                b.leftSon = a;
                a.parent = b;
            }
        } else {
            Node a = currentNode.parent.parent;
            Node b = currentNode.parent;
            if (a.parent.leftSon == a) {
                a.parent.leftSon = currentNode;
            } else {
                a.parent.rightSon = currentNode;
            }
            currentNode.parent = a.parent;
            if (b.rightSon == currentNode) {
                b.rightSon = currentNode.leftSon;
                currentNode.leftSon.parent = b;
                currentNode.leftSon = b;
                b.parent = currentNode;
                a.leftSon = currentNode.rightSon;
                currentNode.rightSon.parent = a;
                currentNode.rightSon = a;
                a.parent = currentNode;
            } else {
                b.leftSon = currentNode.rightSon;
                currentNode.rightSon.parent = b;
                currentNode.rightSon = b;
                b.parent = currentNode;
                a.rightSon = currentNode.leftSon;
                currentNode.leftSon.parent = a;
                currentNode.leftSon = a;
                a.parent = currentNode;
            }
        }
        oddCounter++;
        return null; //wrong
    }

    public boolean exists(int x) {
        Node node = find(x);
        return node != null;
    }

}
