package LLRB_tree_ready

case class R() extends Color
case class B() extends Color

sealed abstract class Color {
    def flip: Color = {
        if (this == R()) B()
        else R()
    }
}

case class Empty() extends Red_black_tree
case class Node(l: Red_black_tree, k: BigInt, c: Color, r: Red_black_tree) extends Red_black_tree

sealed abstract class Red_black_tree {
    def left: Red_black_tree = {
        asInstanceOf[Node].l
    }

    def set_left(new_left: Red_black_tree): Red_black_tree = {
        Node(new_left, key, color, right)
    }

    def key: BigInt = {
        asInstanceOf[Node].k
    }

    def set_key(new_key: BigInt): Red_black_tree = {
        Node(left, new_key, color, right)
    }

    def color: Color = {
        if (this == Empty()) B()
        else asInstanceOf[Node].c
    }

    def set_color(new_color: Color): Red_black_tree = {
        if (this == Empty()) Empty()
        else Node(left, key, new_color, right)
    }

    def right: Red_black_tree = {
        asInstanceOf[Node].r
    }

    def set_right(new_right: Red_black_tree) = {
        Node(left, key, color, new_right)
    }

    def size: BigInt = {
        if (this == Empty()) BigInt(0)
        else 1 + left.size + right.size
    }

    def content: Set[BigInt] = {
        if (this == Empty()) Set()
        else left.content ++ Set(key) ++ right.content
    }

    def contains(x: BigInt): Boolean = {
        if (this == Empty()) false
        else if (x < key) left.contains(x)
        else if (x > key) right.contains(x)
        else true
    }

    def rotate_right: Red_black_tree = {
        Node(left.left, left.key, color, Node(left.right, key, left.color, right))
    }

    def rotate_left: Red_black_tree = {
        Node(Node(left, key, right.color, right.left), right.key, color, right.right)
    }

    def flip_cherry: Red_black_tree = {
        Node(left.set_color(left.color.flip), key, color.flip, right.set_color(right.color.flip))
    }

    def single_right_red: Boolean = {
        this != Empty() && left.color == B() && right.color == R() && right.left.color == B()
    }

    def right_left_red: Boolean = {
        this != Empty() && color == B() && left.color == B() && right.color == R() && right.left.color == R()
    }

    def double_left_red: Boolean = {
        this != Empty() && color == B() && left.color == R() && left.left.color == R() && right.color == B()
    }

    def cherry: Boolean = {
        this != Empty() && color == B() && left.color == R() && right.color == R()
    }

    def fixup: Red_black_tree = {
        if (single_right_red) rotate_left
        else if (right_left_red) set_right(right.rotate_right).rotate_left.flip_cherry
        else if (double_left_red) rotate_right.flip_cherry
        else if (cherry) flip_cherry
        else this
    }

    def insert_2(x: BigInt): Red_black_tree = {
        if (this == Empty()) Node(Empty(), x, R(), Empty())
        else if (x < key) set_left(left.insert_2(x)).fixup
        else if (x > key) set_right(right.insert_2(x)).fixup
        else this
    }

    def insert(x: BigInt): Red_black_tree = {
        insert_2(x).set_color(B())
    }

    def red_invariant: Boolean = {
        this == Empty() || color == R() || left.color == R()
    }

    def move_red_left: Red_black_tree = {
        if (right.left.color == B()) flip_cherry
        else flip_cherry.set_right(flip_cherry.right.rotate_right).rotate_left.flip_cherry
    }

    def move_red_right: Red_black_tree = {
        if (left.left.color == B()) flip_cherry
        else flip_cherry.rotate_right.set_right(flip_cherry.rotate_right.right.rotate_left).flip_cherry
    }

    def min_key: BigInt = {
        if (left == Empty()) key
        else left.min_key
    }

    def delete_2(x: BigInt): Red_black_tree = {
        if (this == Empty()) Empty()
        else if (x < key && !left.red_invariant) move_red_left.set_left(move_red_left.left.delete_2(x)).fixup
        else if (x < key) set_left(left.delete_2(x)).fixup
        else if (left.color == R()) rotate_right.set_right(rotate_right.right.delete_2(x)).fixup
        else if (key == x && right == Empty()) Empty()
        else if (!right.red_invariant && (key != x || (left != Empty() && left.left.color == R()))) move_red_right.set_right(move_red_right.right.delete_2(x)).fixup
        else if (key != x) set_right(right.delete_2(x)).fixup
        else if (!right.red_invariant) move_red_right.set_key(right.min_key).set_right(move_red_right.right.delete_2(right.min_key)).fixup
        else set_key(right.min_key).set_right(right.delete_2(right.min_key)).fixup
    }

    def delete(x: BigInt): Red_black_tree = {
        if (this != Empty() && color == B() && left.color == B()) set_color(R()).delete_2(x).set_color(B())
        else delete_2(x).set_color(B())
    }
}
