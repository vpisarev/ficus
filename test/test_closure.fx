/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// tests for closures

from UTest import *

TEST("closure.no_inner_fn", fun()
{
    fun f(i: int) { i + 1 }
    fun get_f() { f }
    fun test()
    {
        val pf = get_f()
        string(pf(0))
    }

    EXPECT_EQ(test(), "1")
})

TEST("closure.inner_fn_no_fv", fun()
{
    fun f(i: int) { i + 1 }
    fun get_f() { f }
    val pf = get_f();

    EXPECT_EQ(pf(0), 1)
})

TEST("closure.inner_fn_in_global_block", fun()
{
    val res = {
        val inner_res = 1
        fun f() = inner_res + 1
        f()
    }

    EXPECT_EQ(res, 2)
})

TEST("closure.inner_fn_ref", fun()
{
    fun container()
    {
        var counter = 5
        fun get() = counter
        fun get_f() = get
        fun dump() {
            val f = get_f()
            val v = f()
            v
        }
        dump
    }

    val c = container()
    EXPECT_EQ(c(), 5)
})

TEST("closure.inner_fn_with_fv_var", fun()
{
    var counter = 0
    fun get_f(j: int)
    {
        fun f(i: int) { counter += i + j }
        f
    }

    val pf = get_f(1)
    pf(2)
    EXPECT_EQ(counter, 3)
    pf(3)
    EXPECT_EQ(counter, 7)
})

TEST("closure.inner_fn_with_fv", fun()
{
    fun get_f(j: int)
    {
        fun f(i: int) {i + j}
        f
    }
    val pf = get_f(1)
    EXPECT_EQ(pf(0), 1)
})

TEST("closure.inner_recursive_no_fv", fun()
{
    fun test(a: int) {
        val b = a
        fun f1(x: int) = if x == 1 {f2(x - 1)} else {x}
        fun f2(x: int) = if x == 1 {f1(x - 2)} else {x}
        f1(b)
    }

    EXPECT_EQ(test(0), 0)
    EXPECT_EQ(test(1), 0)
    EXPECT_EQ(test(2), 2)
})

TEST("closure.recursive_1", fun()
{
    fun outer(a: int, b: int)
    {
        fun foo(ii: int): int =
            if ii == 1 {
                1
            } else {
                a + bar(ii - 1)
            }

        fun bar(ii: int): int =
            if ii == 1 {
                -1
            } else {
                b + foo(ii - 1)
            }
        foo(a + b)
    }

    EXPECT_EQ(outer(1, 2), 4)
})

TEST("closure.recursive_2", fun()
{
    fun outer(a: int, b: int)
    {
        fun no_fv(i: int) = i+1

        fun foo(ii: int): int =
            if ii == 1 {
                1 + no_fv(a)
            } else {
                a + bar(ii - 1)
            }

        fun bar(ii: int): int =
            if ii == 1 {
                -1 - no_fv(b)
            } else {
                b + foo(ii - 1)
            }

        foo(a + b)
    }

    EXPECT_EQ(outer(1, 2), 6)
})

TEST("closure.recursive_3", fun()
{
    fun outer(a: int, b: int)
    {
        fun no_fv(i: int) = i + 1
        fun fv(i: int) = i + a

        fun foo(ii: int): int =
            if ii == 1 {
                1 + no_fv(a)
            } else {
                foo(a) + bar(ii - 1)
            }

        fun bar(ii: int): int =
            if ii == 1 {
                -1 - no_fv(b) + fv(b)
            } else {
                b + foo(ii - 1)
            }
        foo(a + b)
    }

    EXPECT_EQ(outer(1, 2), 8)
})

TEST("closure.kfor_0_no_kfor_fv", fun()
{
    fun outer_direct(a: int)
    {
        val hs = [parallel for i <- 0:10 {
            fun kfor_inner()
            {
                fun h(ii: int): int = ii + a
                h
            }
            kfor_inner()
        }]
        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_direct(1000), [: for i <- 0:10 {1100} :])
})

TEST("closure.kfor_1_fv", fun()
{
    fun outer_direct_2(a: int)
    {
        fun fn_fv(j: int) = a + j
        val hs = [parallel for i <- 0:10 {
            fun kfor_inner()
            {
                fun h(ii: int) = ii + i + a + fn_fv(ii)
                h
            }
            kfor_inner()
        }]

        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_direct_2(1000), [: for i <- 0:10 {2200 + i} :])
})

TEST("closure.kfor_1", fun()
{
    fun outer_direct(a: int)
    {
        val hs = [parallel for i <- 0:10 {
            fun kfor_inner()
            {
                fun h(ii: int) = ii + i + a
                h
            }
            kfor_inner()
        }]

        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_direct(1000), [: for i <- 0:10 {1100 + i} :])
})

TEST("closure.kfor_2", fun()
{
    fun outer_direct_k(a: int)
    {
        fun fn_fv(j: int) = a + j

        val hs = [parallel for i <- 0:10 {
            fun kfor_inner(k: int)
            {
                fun h(ii: int) = ii + i + a + k + fn_fv(ii)
                h
            }
            kfor_inner(i + 1)
        }]

        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_direct_k(1000), [: for i <- 0:10 {2200 + 2 * i + 1} :])
})

TEST("closure.kfor_3", fun()
{
    fun outer_inner_val(a: int)
    {
        val hs = [parallel for i <- 0:10 {
            val i_value = i
            fun h(ii: int) = ii + i_value + a
            h
        }]

        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_inner_val(1000), [: for i <- 0:10 {1100 + i} :])
})

TEST("closure.kfor_4", fun()
{
    fun outer_inner_var(a: int)
    {
        val hs = [parallel for i <- 0:10 {
            var i_value = i
            fun h(ii: int) = ii + i_value + a
            h
        }]

        [: for f <- hs {f(100)} :]
    }

    EXPECT_EQ(outer_inner_var(1000), [: for i <- 0:10 {1100 + i} :])
})

TEST("closure.kfor_5", fun()
{
    fun outer_outer_var(a: int)
    {
        var i_value = 0;
        val hs = [parallel for i <- 0:10 {
            i_value = i
            fun h(ii: int) = ii + i_value + a
            h
        }]
        (i_value, [: for f <- hs {f(100)} :])
    }

    val result = outer_outer_var(1000)
    EXPECT_EQ(result.1, [: for i <- 0:10 {1100 + result.0} :])
})

TEST("closure.object", fun()
{
    fun closure_object(a: int)
    {
        var res: int list = []
        val hs = [parallel for i <- 0:10 {
            val i_value = i
            fun h(ii: int) { res = (i_value + ii) :: res }
            h
        }]

        for f <- hs {f(100)}
        var s = ""
        for i <- res {
            s += string(i) + ";"
        }
        s
    }

    EXPECT_EQ(closure_object(100), "109;108;107;106;105;104;103;102;101;100;")
})

TEST("closure.object_in_tuple", fun()
{
    fun closure_object(a: int)
    {
        var res: (int list, float list) = ([], [])
        val hs = [parallel for i <- 0:10 {
            val i_value = i;
            fun h(ii: int) {res = ((i_value + ii) :: res.0, (i :> float) :: res.1) }
            h
        }]

        for f <- hs {f(100)}

        var s = ""
        for i <- res.0 {
            s += string(i) + ";"
        }
        s
    }

    EXPECT_EQ(closure_object(1000), "109;108;107;106;105;104;103;102;101;100;")
})

TEST("closure.middle_empty_closure", fun()
{
    fun test(a: int) {
        val b = a
        fun foo() {
            fun bar() = a * 2
            fun baz() = bar() * 2
            baz()
        }
        foo()
    }

    EXPECT_EQ(test(0), 0)
    EXPECT_EQ(test(1), 4)
})

TEST("closure.declare_alias", fun ()
{
    val my_own_max_function = fun(a: int, b: int) { max(a, b) }

    EXPECT_EQ(my_own_max_function(2, 3), 3)
})

TEST("closure.handmade_functions", fun()
{
    fun g(a: int) = fun(b: int) { a + b }

    EXPECT_EQ(g(2)(3), 5)
})

TEST("closure.max_function", fun()
{
    val x = max(3, 6)
    EXPECT_EQ(x, 6)

    val max_6 = fun(a: int) { max(a, 6) }

    EXPECT_EQ(max_6(1), 6)
    EXPECT_EQ(max_6(7), 7)
})

TEST("closure.max_function_depending_on_val", fun()
{
    val threshold = 10
    val max_10 = fun(a: int) { max(a, threshold) }

    EXPECT_EQ(max_10(1), 10)
    EXPECT_EQ(max_10(70), 70)
})

TEST("closure.max_function_depending_on_var", fun()
{
    var threshold = 10
    val max_10 = fun(a: int) { max(a, threshold) }

    EXPECT_EQ(max_10(1), 10)
    EXPECT_EQ(max_10(70), 70)

    threshold = 11
    EXPECT_EQ(max_10(1), 11)
    EXPECT_EQ(max_10(70), 70)
})
