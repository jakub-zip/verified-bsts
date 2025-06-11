package AVL_tree_cached_ready

case class Empty() extends Binary_tree_cached
case class Node(left: Binary_tree_cached, key: BigInt, cached_height: BigInt, right: Binary_tree_cached) extends Binary_tree_cached

sealed abstract class Binary_tree_cached {
    def height_cached: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(_, _, h, _) => h
        }
    }

    def sign_cached: BigInt = {
        this match {
            case Empty()          => BigInt(0)
            case Node(l, _, _, r) => r.height_cached - l.height_cached
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
            case Node(l, k, _, r) =>
                if (x < k) l.contains(x)
                else if (x > k) r.contains(x)
                else true
        }
    }

    def recalculate_height: Binary_tree_cached = {
        this match {
            case Empty() => Empty()
            case Node(l, k, _, r) => Node(l, k, 1 + l.height_cached.max(r.height_cached), r)
        }
    }

    def insert(x: BigInt): Binary_tree_cached = {
        this match {
            case Empty() => Node(Empty(), x, 1, Empty())
            case Node(l, k, _, r) => {
                if (x < k) Node(l.insert(x), k, -1, r).recalculate_height.rebalance
                else if (x > k) Node(l, k, -1, r.insert(x)).recalculate_height.rebalance
                else this
            }

        }
    }

    def min_key: BigInt = {
        this match {
            case Node(Empty(), k, _, _) => k
            case Node(l, _, _, _)       => l.min_key
        }
    }

    def delete(x: BigInt): Binary_tree_cached = {
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
    }

    def rotate_right: Binary_tree_cached = {
        this match {
            case Node(l, k, _, r) => {
                l match {
                    case Node(ll, lk, _, lr) => {
                        Node(ll, lk, -1, Node(lr, k, -1, r).recalculate_height).recalculate_height
                    }
                }
            }
        }
    }

    def rotate_left: Binary_tree_cached = {
        this match {
            case Node(l, k, _, r) => {
                r match {
                    case Node(rl, rk, _, rr) => {
                        Node(Node(l, k, -1, rl).recalculate_height, rk, -1, rr).recalculate_height
                    }
                }
            }
        }
    }

    def rotate_rl: Binary_tree_cached = {
        this match {
            case Node(l, k, _, r) => {
                Node(l, k, -1, r.rotate_right).recalculate_height.rotate_left
            }
        }
    }

    def rotate_lr: Binary_tree_cached = {
        this match {
            case Node(l, k, _, r) => {
                Node(l.rotate_left, k, -1, r).recalculate_height.rotate_right
            }
        }
    }

    def rebalance: Binary_tree_cached = {
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
    }
}
