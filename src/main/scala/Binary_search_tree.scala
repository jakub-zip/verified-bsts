package Binary_search_tree

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import stainless.math._

case class Empty() extends Binary_tree
case class Node(left: Binary_tree, key: BigInt, right: Binary_tree) extends Binary_tree

sealed abstract class Binary_tree {
    def height: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => 1 + max(l.height, r.height)
        }
    }.ensuring(res => res >= 0)

    def size: BigInt = {
        this match {
            case Empty()       => BigInt(0)
            case Node(l, _, r) => 1 + l.size + r.size
        }
    }.ensuring(res => res >= 0)

    def bst: Boolean = {
        decreases(height, size)
        this match {
            case Empty() => true
            case Node(l, k, r) =>
                forall((x: BigInt) => (l.content.contains(x) ==> (x < k))) &&
                forall((x: BigInt) => (r.content.contains(x) ==> (k < x))) &&
                l.bst && r.bst
        }
    }

    def content: Set[BigInt] = {
        this match {
            case Empty() => Set()
            case Node(l, k, r) =>
                l.content ++ Set(k) ++ r.content
        }
    }

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
        decreases(size, height)
        require(bst)
        this match {
            case Empty() => Node(Empty(), x, Empty())
            case Node(l, k, r) =>
                if (x < k) Node(l.insert(x), k, r)
                else if (x > k) Node(l, k, r.insert(x))
                else this
        }
    }.ensuring(res =>
        res.content == content ++ Set(x) &&
        res.bst
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
        require(bst)
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
    }.ensuring(res =>
        res.content == content -- Set(x) &&
        res.bst
    )
}
