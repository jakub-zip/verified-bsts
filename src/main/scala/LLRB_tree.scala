package LLRB_tree

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math._

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
        require(this != Empty())
        asInstanceOf[Node].l
    }

    def set_left(new_left: Red_black_tree): Red_black_tree = {
        require(this != Empty())
        Node(new_left, key, color, right)
    }.ensuring(res => res.left == new_left)

    def key: BigInt = {
        require(this != Empty())
        asInstanceOf[Node].k
    }

    def set_key(new_key: BigInt): Red_black_tree = {
        require(this != Empty())
        Node(left, new_key, color, right)
    }.ensuring(res => res.key == new_key)

    def color: Color = {
        if (this == Empty()) B()
        else asInstanceOf[Node].c
    }

    def set_color(new_color: Color): Red_black_tree = {
        require(this != Empty() || new_color != R())
        if (this == Empty()) Empty()
        else Node(left, key, new_color, right)
    }.ensuring(res => res.color == new_color)

    def right: Red_black_tree = {
        require(this != Empty())
        asInstanceOf[Node].r
    }

    def set_right(new_right: Red_black_tree) = {
        require(this != Empty())
        Node(left, key, color, new_right)
    }.ensuring(res => res.right == new_right)

    def size: BigInt = {
        if (this == Empty()) BigInt(0)
        else 1 + left.size + right.size
    }.ensuring(res => res >= 0)

    def content: Set[BigInt] = {
        if (this == Empty()) Set()
        else left.content ++ Set(key) ++ right.content
    }

    def bst: Boolean = {
        if (this == Empty()) true
        else
            forall((x: BigInt) => left.content.contains(x) ==> (x < key)) &&
            forall((x: BigInt) => right.content.contains(x) ==> (key < x)) &&
            left.bst && right.bst
    }

    def right_child_black: Boolean = {
        if (this == Empty()) true
        else right.color == B() && left.right_child_black && right.right_child_black
    }

    def red_has_left_black: Boolean = {
        if (this == Empty()) true
        else ((color == R()) ==> (left.color == B())) && left.red_has_left_black && right.red_has_left_black
    }

    def red_has_left_black_but_root: Boolean = {
        if (this == Empty()) true
        else left.red_has_left_black && right.red_has_left_black
    }

    def step: BigInt = {
        if (color == B()) BigInt(1)
        else BigInt(0)
    }

    def min_black_height: BigInt = {
        if (this == Empty()) BigInt(1)
        else min(left.min_black_height, right.min_black_height) + step
    }

    def max_black_height: BigInt = {
        if (this == Empty()) BigInt(1)
        else max(left.max_black_height, right.max_black_height) + step
    }

    def black_height_equal: Boolean = {
        if (this == Empty()) true
        else min_black_height == max_black_height && left.black_height_equal && right.black_height_equal
    }

    def black_height: BigInt = {
        require(black_height_equal)
        if (this == Empty()) BigInt(1)
        else left.black_height + step
    }.ensuring(res => res == min_black_height && res == max_black_height && res >= 0)

    def llrb: Boolean = {
        bst && black_height_equal && red_has_left_black && right_child_black
    }

    def llrb_relaxed: Boolean = {
        bst && black_height_equal && red_has_left_black_but_root && right_child_black
    }

    def contains(x: BigInt): Boolean = {
        require(bst)
        if (this == Empty()) false
        else if (x < key) left.contains(x)
        else if (x > key) right.contains(x)
        else true
    }.ensuring(res => (res == (content.contains(x))))

    def rotate_right: Red_black_tree = {
        require(this != Empty())
        require(left != Empty())
        require(left.color == R())
        require(bst)
        require(black_height_equal)
        Node(left.left, left.key, color, Node(left.right, key, left.color, right))
    }.ensuring(res =>
        content == res.content &&
        res.bst &&
        res.black_height_equal &&
        black_height == res.black_height
    )

    def rotate_left: Red_black_tree = {
        require(this != Empty())
        require(right != Empty())
        require(right.color == R())
        require(bst)
        require(black_height_equal)
        Node(Node(left, key, right.color, right.left), right.key, color, right.right)
    }.ensuring(res =>
        content == res.content &&
        res.bst &&
        res.black_height_equal &&
        black_height == res.black_height
    )

    def flip_cherry: Red_black_tree = {
        require(this != Empty())
        require(left != Empty())
        require(right != Empty())
        require(left.color == right.color)
        require(color != left.color)
        require(bst)
        require(black_height_equal)
        Node(left.set_color(left.color.flip), key, color.flip, right.set_color(right.color.flip))
    }.ensuring(res =>
        content == res.content &&
        res.bst &&
        res.black_height_equal &&
        black_height == res.black_height
    )

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

    def unfixable_case_1: Boolean = {
        this != Empty() && color == R() && right.color == R() && right.left.color == R()
    }

    def unfixable_case_2: Boolean = {
        this != Empty() && color == R() && left.color == R() && right.color == R()
    }

    def unfixable_case_3: Boolean = {
        this != Empty() && color == R() && left.color == R() && left.left.color == R()
    }

    def fixup: Red_black_tree = {
        require(bst)
        require(black_height_equal)
        require(this == Empty() || (left.llrb_relaxed && right.llrb) || (left.llrb && right.llrb_relaxed))
        require(!unfixable_case_1)
        require(!unfixable_case_2)
        require(!unfixable_case_3)
        if (single_right_red) rotate_left
        else if (right_left_red) set_right(right.rotate_right).rotate_left.flip_cherry
        else if (double_left_red) rotate_right.flip_cherry
        else if (cherry) flip_cherry
        else this
    }.ensuring(res =>
        content == res.content &&
        black_height == res.black_height &&
        res.llrb_relaxed
    )

    def insert_2(x: BigInt): Red_black_tree = {
        decreases(size)
        require(llrb)
        if (this == Empty()) Node(Empty(), x, R(), Empty())
        else if (x < key) set_left(left.insert_2(x)).fixup
        else if (x > key) set_right(right.insert_2(x)).fixup
        else this
    }.ensuring(res =>
        res.content == content ++ Set(x) &&
        black_height == res.black_height &&
        res.llrb_relaxed
    )

    def insert(x: BigInt): Red_black_tree = {
        require(llrb)
        insert_2(x).set_color(B())
    }.ensuring(res =>
        res.content == content ++ Set(x) &&
        (black_height == res.black_height || black_height + 1 == res.black_height) &&
        res.llrb
    )

    def red_invariant: Boolean = {
        this == Empty() || color == R() || left.color == R()
    }

    def move_red_left: Red_black_tree = {
        require(llrb)
        require(this != Empty())
        require(left != Empty())
        require(right != Empty())
        require(left.color == B())
        require(right.color == B())
        require(color == R())
        require(!left.red_invariant)
        if (right.left.color == B()) flip_cherry
        else flip_cherry.set_right(flip_cherry.right.rotate_right).rotate_left.flip_cherry
    }.ensuring(res =>
        content == res.content &&
        res.bst &&
        res.red_has_left_black &&
        black_height == res.black_height &&
        res.right.llrb &&
        res.left.llrb &&
        res.left.red_invariant
    )

    def move_red_right: Red_black_tree = {
        require(llrb)
        require(this != Empty())
        require(left != Empty())
        require(right != Empty())
        require(left.color == B())
        require(right.color == B())
        require(color == R())
        require(!right.red_invariant)
        if (left.left.color == B()) flip_cherry
        else flip_cherry.rotate_right.set_right(flip_cherry.rotate_right.right.rotate_left).flip_cherry
    }.ensuring(res =>
        content == res.content &&
        res.bst &&
        res.red_has_left_black &&
        black_height == res.black_height &&
        res.right.llrb &&
        res.left.llrb &&
        res.right.red_invariant
    )

    def min_key: BigInt = {
        require(isInstanceOf[Node])
        require(bst)
        if (left == Empty()) key
        else left.min_key
    }.ensuring(res =>
        content.contains(res) &&
        forall((x: BigInt) => (content.contains(x) ==> (res <= x)))
    )

    def delete_2(x: BigInt): Red_black_tree = {
        decreases(size)
        require(llrb)
        require(red_invariant)
        if (this == Empty()) Empty()
        else if (x < key && !left.red_invariant) {
            assert(left.black_height >= 2)
            move_red_left.set_left(move_red_left.left.delete_2(x)).fixup
        } else if (x < key) set_left(left.delete_2(x)).fixup
        else if (left.color == R()) rotate_right.set_right(rotate_right.right.delete_2(x)).fixup
        else if (key == x && right == Empty()) {
            assert(black_height == 1)
            Empty()
        } else if (!right.red_invariant && (key != x || (left != Empty() && left.left.color == R()))) {
            assert(right.black_height >= 2)
            move_red_right.set_right(move_red_right.right.delete_2(x)).fixup
        } else if (key != x) set_right(right.delete_2(x)).fixup
        else if (!right.red_invariant) {
            assert(right.black_height >= 2)
            move_red_right.set_key(right.min_key).set_right(move_red_right.right.delete_2(right.min_key)).fixup
        } else set_key(right.min_key).set_right(right.delete_2(right.min_key)).fixup
    }.ensuring(res =>
        res.content == content -- Set(x) &&
        black_height == res.black_height &&
        res.llrb_relaxed
    )

    def delete(x: BigInt): Red_black_tree = {
        require(llrb)
        if (this != Empty() && color == B() && left.color == B()) set_color(R()).delete_2(x).set_color(B())
        else delete_2(x).set_color(B())
    }.ensuring(res =>
        res.content == content -- Set(x) &&
        (black_height - res.black_height <= 1) &&
        res.llrb
    )
}
