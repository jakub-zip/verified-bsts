package AVL_tree

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math._

case class Empty() extends Binary_tree
case class Node(left: Binary_tree, key: BigInt, right: Binary_tree) extends Binary_tree

sealed abstract class Binary_tree {
    def size: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => 1 + l.size + r.size
        }
    }.ensuring(res => res >= 0)

    def height: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => 1 + max(l.height, r.height)
        }
    }.ensuring(res => res >= 0)

    def sign: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => r.height - l.height
        }
    }

    def content: Set[BigInt] = {
        this match {
            case Empty() => Set()
            case Node(l, k, r) =>
                l.content ++ Set(k) ++ r.content
        }
    }

    def bst: Boolean = {
        this match {
            case Empty() => true
            case Node(l, k, r) =>
                forall((x: BigInt) => (l.content.contains(x) ==> (x < k))) &&
                forall((x: BigInt) => (r.content.contains(x) ==> (k < x))) &&
                l.bst && r.bst
        }
    }

    def height_balanced_but_root: Boolean = {
        this match {
            case Empty()       => true
            case Node(l, _, r) => l.height_balanced && r.height_balanced
        }
    }

    def height_balanced: Boolean = {
        this match {
            case Empty() => true
            case Node(l, _, r) =>
                abs(sign) <= 1 &&
                l.height_balanced && r.height_balanced
        }
    }.ensuring(res => res ==> height_balanced_but_root)

    def contains(x: BigInt): Boolean = {
        require(bst)
        this match {
            case Empty() => false
            case Node(l, k, r) =>
                if (x < k) l.contains(x)
                else if (x > k) r.contains(x)
                else true
        }
    }.ensuring(res => (res == (content.contains(x))))

    def insert(x: BigInt): Binary_tree = {
        decreases(height)
        require(bst)
        require(height_balanced)
        this match {
            case Empty() => Node(Empty(), x, Empty())
            case Node(l, k, r) =>
                if (x < k) Node(l.insert(x), k, r).rebalance
                else if (x > k) Node(l, k, r.insert(x)).rebalance
                else this
        }
    }.ensuring(res =>
        res.content == content ++ Set(x) &&
        res.bst &&
        (height == res.height || height + 1 == res.height) &&
        res.height_balanced
    )

    def min_key: BigInt = {
        require(isInstanceOf[Node])
        require(bst)
        this match {
            case Node(Empty(), k, _) => k
            case Node(l, _, _)       => l.min_key
        }
    }.ensuring(res =>
        content.contains(res) &&
            forall((x: BigInt) => (content.contains(x) ==> (res <= x)))
    )

    def delete(x: BigInt): Binary_tree = {
        decreases(height)
        require(bst)
        require(height_balanced)
        this match {
            case Empty() => Empty()
            case Node(l, k, r) => {
                if (x == k) {
                    (l, r) match {
                        case (Empty(), Empty()) => Empty()
                        case (l, Empty())       => l
                        case (Empty(), r)       => r
                        case (l, r) => {
                            val successor = r.min_key
                            Node(l, successor, r.delete(successor)).rebalance
                        }
                    }
                } else if (x < k) Node(l.delete(x), k, r).rebalance
                else Node(l, k, r.delete(x)).rebalance
            }
        }
    }.ensuring(res =>
        res.content == content -- Set(x) &&
        res.bst &&
        (height == res.height || height - 1 == res.height) &&
        res.height_balanced
    )

    def right_rotatable: Boolean = {
        isInstanceOf[Node] && asInstanceOf[Node].left.isInstanceOf[Node]
    }

    def rotate_right: Binary_tree = {
        require(right_rotatable)
        this match {
            case Node(l, k, r) => {
                l match {
                    case Node(ll, lk, lr) => {
                        Node(ll, lk, Node(lr, k, r))
                    }
                }
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        ((height_balanced_but_root && sign == -2 && asInstanceOf[Node].left.sign < 1) ==> res.height_balanced)
    )

    def left_rotatable: Boolean = {
        isInstanceOf[Node] && asInstanceOf[Node].right.isInstanceOf[Node]
    }

    def rotate_left: Binary_tree = {
        require(left_rotatable)
        this match {
            case Node(l, k, r) => {
                r match {
                    case Node(rl, rk, rr) => {
                        Node(Node(l, k, rl), rk, rr)
                    }
                }
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        ((height_balanced_but_root && sign == 2 && asInstanceOf[Node].right.sign > -1) ==> res.height_balanced)
    )

    def rl_rotatable: Boolean = {
        left_rotatable && asInstanceOf[Node].right.right_rotatable
    }

    def rotate_rl: Binary_tree = {
        require(rl_rotatable)
        this match {
            case Node(l, k, r) => {
                Node(l, k, r.rotate_right).rotate_left
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        ((height_balanced_but_root && sign == 2 && asInstanceOf[Node].right.sign == -1) ==> res.height_balanced)
    )

    def lr_rotatable: Boolean = {
        right_rotatable && asInstanceOf[Node].left.left_rotatable
    }

    def rotate_lr: Binary_tree = {
        require(lr_rotatable)
        this match {
            case Node(l, k, r) => {
                Node(l.rotate_left, k, r).rotate_right
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        ((height_balanced_but_root && sign == -2 && asInstanceOf[Node].left.sign == 1) ==> res.height_balanced)
    )

    def rebalance: Binary_tree = {
        require(height_balanced || (height_balanced_but_root && abs(sign) <= 2))
        this match {
            case Empty() => this
            case n @ Node(l, k, r) => {
                sign match {
                    case s if s == 2 => {
                        r.sign match {
                            case rs if rs == 0 || rs == 1 => n.rotate_left
                            case rs if rs == -1           => n.rotate_rl
                        }
                    }
                    case s if s == -2 => {
                        l.sign match {
                            case ls if ls == 0 || ls == -1 => n.rotate_right
                            case ls if ls == 1             => n.rotate_lr
                        }
                    }
                    case _ => this
                }
            }
        }
    }.ensuring(res =>
        content == res.content &&
        (bst ==> res.bst) &&
        res.height_balanced
    )
}
