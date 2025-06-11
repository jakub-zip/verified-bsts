package Binary_search_tree_ready

case class Empty() extends Binary_tree
case class Node(left: Binary_tree, key: BigInt, right: Binary_tree) extends Binary_tree

sealed abstract class Binary_tree {
    def content: Set[BigInt] = {
        this match {
            case Empty() => Set()
            case Node(l, k, r) =>
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
                if (x < k) Node(l.insert(x), k, r)
                else if (x > k) Node(l, k, r.insert(x))
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
                            Node(l, successor, r.delete(successor))
                        }
                    }
                } else if (x < k) Node(l.delete(x), k, r)
                else Node(l, k, r.delete(x))
            }
        }
    }
}
