package AVL_tree_ready

import org.scalatest.funsuite.AnyFunSuite

class AVL_tree_ready_test extends AnyFunSuite {
    test("Simple insertion") {
        var t: Binary_tree = Empty()
        assert(t.content == Set())

        t = t.insert(42)
        assert(t.content == Set(42))
        
        t = t.insert(42)
        assert(t.content == Set(42))

        t = t.insert(123)
        assert(t.content == Set(42, 123))
    }

    test("Simple contains") {
        var t: Binary_tree = Empty()
        assert(!t.contains(42))

        t = t.insert(42)
        assert(t.contains(42))
        assert(!t.contains(123))

        t = t.insert(123)
        assert(t.contains(123))
        assert(t.contains(42))
    }

    test("Simple delete") {
        var t: Binary_tree = Empty()
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
}