package AVL_tree_ready

case class Empty() extends Binary_tree
case class Node(left: Binary_tree, key: BigInt, right: Binary_tree) extends Binary_tree

sealed abstract class Binary_tree {
    def height: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => 1 + l.height.max(r.height)
        }
    }

    def sign: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => r.height - l.height
        }
    }

    def content: Set[BigInt] = {
        this match {
            case Empty() => Set()
            case Node(l, k, _, r) =>
                l.content ++ Set(k) ++ r.content
        }
    }

    def contains(x: BigInt): Boolean = {
        this match {
            case Empty() => false
            case Node(l, k, r) =>
                if (x < k) l.contains(x)
                else if (x > k) r.contains(x)
                else true
        }
    }

    def insert(x: BigInt): Binary_tree = {
        this match {
            case Empty() => Node(Empty(), x, Empty())
            case Node(l, k, r) =>
                if (x < k) Node(l.insert(x), k, r).rebalance
                else if (x > k) Node(l, k, r.insert(x)).rebalance
                else this
        }
    }

    def min_key: BigInt = {
        this match {
            case Node(Empty(), k, _) => k
            case Node(l, _, _)       => l.min_key
        }
    }

    def delete(x: BigInt): Binary_tree = {
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
    }

    def rotate_right: Binary_tree = {
        this match {
            case Node(l, k, r) => {
                l match {
                    case Node(ll, lk, lr) => {
                        Node(ll, lk, Node(lr, k, r))
                    }
                }
            }
        }
    }

    def rotate_left: Binary_tree = {
        this match {
            case Node(l, k, r) => {
                r match {
                    case Node(rl, rk, rr) => {
                        Node(Node(l, k, rl), rk, rr)
                    }
                }
            }
        }
    }

    def rotate_rl: Binary_tree = {
        this match {
            case Node(l, k, r) => {
                Node(l, k, r.rotate_right).rotate_left
            }
        }
    }

    def rotate_lr: Binary_tree = {
        this match {
            case Node(l, k, r) => {
                Node(l.rotate_left, k, r).rotate_right
            }
        }
    }

    def rebalance: Binary_tree = {
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
    }
}
