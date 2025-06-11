package AVL_tree_cached_ready

import org.scalatest.funsuite.AnyFunSuite

class AVL_tree_cached_ready_test extends AnyFunSuite {
    test("Simple insertion") {
        var t: Binary_tree_cached = Empty()
        assert(t.content == Set())

        t = t.insert(42)
        assert(t.content == Set(42))
        
        t = t.insert(42)
        assert(t.content == Set(42))

        t = t.insert(123)
        assert(t.content == Set(42, 123))
    }

    test("Simple contains") {
        var t: Binary_tree_cached = Empty()
        assert(!t.contains(42))

        t = t.insert(42)
        assert(t.contains(42))
        assert(!t.contains(123))

        t = t.insert(123)
        assert(t.contains(123))
        assert(t.contains(42))
    }

    test("Simple delete") {
        var t: Binary_tree_cached = Empty()
        assert(t.content == Set())

        t = t.insert(42)
        assert(t.content == Set(42))

        t = t.delete(42)
        assert(t.content == Set())

        t = t.insert(42)
        t = t.insert(123)
        assert(t.content == Set(42, 123))

        t = t.delete(42)
        assert(t.content == Set(123))

        t = t.delete(123)
        assert(t.content == Set())
    }

    test("Insert million inorder elements") {
        var t: Binary_tree_cached = Empty()
        val lst = (0 until 1000000).toList

        lst.foreach(x => t = t.insert(x))
        assert(t.content == lst.toSet)
    }

    test("Insert million random elements") {
        var t: Binary_tree_cached = Empty()
        val lst = scala.util.Random.shuffle((0 until 1000000).toList)

        lst.foreach(x => t = t.insert(x))
        assert(t.content == lst.toSet)
    }
}