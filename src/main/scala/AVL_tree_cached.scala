package AVL_tree_cached

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math._

case class Empty() extends Binary_tree_cached
case class Node(left: Binary_tree_cached, key: BigInt, cached_height: BigInt, right: Binary_tree_cached) extends Binary_tree_cached

sealed abstract class Binary_tree_cached {
    def size: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(l, _, _, r) => 1 + l.size + r.size
        }
    }.ensuring(res => res >= 0)

    def height: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(l, _, _, r) => 1 + max(l.height, r.height)
        }
    }.ensuring(res => res >= 0)

    def height_filled_correctly: Boolean = {
        this match {
            case Empty() => true
            case Node(l, _, h, r) => {
                h == height &&
                l.height_filled_correctly && r.height_filled_correctly
            }
        }
    }

    def height_cached: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(_, _, h, _) => h
        }
    }.ensuring(res => (height_filled_correctly ==> (res == height)))

    def sign: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(l, _, _, r) => r.height - l.height
        }
    }

    def sign_cached: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(l, _, _, r) => r.height_cached - l.height_cached
        }
    }.ensuring(res => (height_filled_correctly ==> (res == sign)))

    def content: Set[BigInt] = {
        this match {
            case Empty() => Set()
            case Node(l, k, _, r) =>
                l.content ++ Set(k) ++ r.content
        }
    }

    def bst: Boolean = {
        this match {
            case Empty() => true
            case Node(l, k, _, r) =>
                forall((x: BigInt) => (l.content.contains(x) ==> (x < k))) &&
                forall((x: BigInt) => (r.content.contains(x) ==> (k < x))) &&
                l.bst && r.bst
        }
    }

    def height_balanced_but_root: Boolean = {
        this match {
            case Empty()          => true
            case Node(l, _, _, r) => l.height_balanced && r.height_balanced
        }
    }

    def height_balanced: Boolean = {
        this match {
            case Empty() => true
            case Node(l, _, _, r) =>
                abs(sign) <= 1 &&
                l.height_balanced && r.height_balanced
        }
    }.ensuring(res => res ==> height_balanced_but_root)

    def contains(x: BigInt): Boolean = {
        require(bst)
        this match {
            case Empty() => false
            case Node(l, k, _, r) =>
                if (x < k) l.contains(x)
                else if (x > k) r.contains(x)
                else true
        }
    }.ensuring(res => (res == (content.contains(x))))

    def height_filled_correctly_but_root: Boolean = {
        this match {
            case Empty() => false
            case Node(l, _, _, r) => l.height_filled_correctly && r.height_filled_correctly
        }
    }

    def recalculate_height: Binary_tree_cached = {
        this match {
            case Empty() => Empty()
            case Node(l, k, _, r) => Node(l, k, 1 + max(l.height_cached, r.height_cached), r)
        }
    }.ensuring(res => (height_filled_correctly_but_root ==> res.height_filled_correctly))

    def insert(x: BigInt): Binary_tree_cached = {
        decreases(height)
        require(bst)
        require(height_filled_correctly)
        require(height_balanced)
        this match {
            case Empty() => Node(Empty(), x, 1, Empty())
            case Node(l, k, _, r) => {
                if (x < k) Node(l.insert(x), k, -1, r).recalculate_height.rebalance
                else if (x > k) Node(l, k, -1, r.insert(x)).recalculate_height.rebalance
                else this
            }

        }
    }.ensuring(res =>
        res.content == content ++ Set(x) &&
        res.bst &&
        (height == res.height || height + 1 == res.height) &&
        res.height_balanced &&
        res.height_filled_correctly
    )

    def min_key: BigInt = {
        require(isInstanceOf[Node])
        require(bst)
        this match {
            case Node(Empty(), k, _, _) => k
            case Node(l, _, _, _)       => l.min_key
        }
    }.ensuring(res =>
        content.contains(res) &&
        forall((x: BigInt) => (content.contains(x) ==> (res <= x)))
    )

    def delete(x: BigInt): Binary_tree_cached = {
        decreases(height)
        require(bst)
        require(height_filled_correctly)
        require(height_balanced)
        this match {
            case Empty() => Empty()
            case Node(l, k, _, r) => {
                if (x == k) {
                    (l, r) match {
                        case (Empty(), Empty()) => Empty()
                        case (l, Empty())       => l
                        case (Empty(), r)       => r
                        case (l, r) => {
                            val successor = r.min_key
                            Node(l, successor, -1, r.delete(successor)).recalculate_height.rebalance
                        }
                    }
                } else if (x < k) Node(l.delete(x), k, -1, r).recalculate_height.rebalance
                else Node(l, k, -1, r.delete(x)).recalculate_height.rebalance
            }
        }
    }.ensuring(res =>
        res.content == content -- Set(x) &&
        res.bst &&
        (height == res.height || height - 1 == res.height) &&
        res.height_balanced &&
        res.height_filled_correctly
    )

    def right_rotatable: Boolean = {
        isInstanceOf[Node] && asInstanceOf[Node].left.isInstanceOf[Node]
    }

    def rotate_right: Binary_tree_cached = {
        require(right_rotatable)
        this match {
            case Node(l, k, _, r) => {
                l match {
                    case Node(ll, lk, _, lr) => {
                        Node(ll, lk, -1, Node(lr, k, -1, r).recalculate_height).recalculate_height
                    }
                }
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        (height_filled_correctly ==> res.height_filled_correctly) &&
        ((height_balanced_but_root && sign == -2 && asInstanceOf[Node].left.sign < 1) ==> res.height_balanced)
    )

    def left_rotatable: Boolean = {
        isInstanceOf[Node] && asInstanceOf[Node].right.isInstanceOf[Node]
    }

    def rotate_left: Binary_tree_cached = {
        require(left_rotatable)
        this match {
            case Node(l, k, _, r) => {
                r match {
                    case Node(rl, rk, _, rr) => {
                        Node(Node(l, k, -1, rl).recalculate_height, rk, -1, rr).recalculate_height
                    }
                }
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        (height_filled_correctly ==> res.height_filled_correctly) &&
        ((height_balanced_but_root && sign == 2 && asInstanceOf[Node].right.sign > -1) ==> res.height_balanced)
    )

    def rl_rotatable: Boolean = {
        left_rotatable && asInstanceOf[Node].right.right_rotatable
    }

    def rotate_rl: Binary_tree_cached = {
        require(rl_rotatable)
        this match {
            case Node(l, k, _, r) => {
                Node(l, k, -1, r.rotate_right).recalculate_height.rotate_left
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        (height_filled_correctly ==> res.height_filled_correctly) &&
        ((height_balanced_but_root && sign == 2 && asInstanceOf[Node].right.sign == -1) ==> res.height_balanced)
    )

    def lr_rotatable: Boolean = {
        right_rotatable && asInstanceOf[Node].left.left_rotatable
    }

    def rotate_lr: Binary_tree_cached = {
        require(lr_rotatable)
        this match {
            case Node(l, k, _, r) => {
                Node(l.rotate_left, k, -1, r).recalculate_height.rotate_right
            }
        }
    }.ensuring(res =>
        (bst ==> res.bst) &&
        (height_filled_correctly ==> res.height_filled_correctly) &&
        ((height_balanced_but_root && sign == -2 && asInstanceOf[Node].left.sign == 1) ==> res.height_balanced)
    )

    def rebalance: Binary_tree_cached = {
        require(height_filled_correctly)
        require(height_balanced || (height_balanced_but_root && abs(sign) <= 2))
        this match {
            case Empty() => this
            case n @ Node(l, k, _, r) => {
                sign_cached match {
                    case s if s == 2 => {
                        r.sign_cached match {
                            case rs if rs == 0 || rs == 1 => n.rotate_left
                            case rs if rs == -1           => n.rotate_rl
                        }
                    }
                    case s if s == -2 => {
                        l.sign_cached match {
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
        res.height_balanced &&
        res.height_filled_correctly
    )
}
