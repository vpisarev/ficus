/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some basic tests

from UTest import *
import Math

import myops

TEST("basic.version", fun()
{
    EXPECT_EQ(f"{__ficus_major__}.{__ficus_minor__}.{__ficus_patchlevel__}", f"{__ficus_version_str__}")
})

TEST("basic.myops.mad_ccode", fun()
{
    EXPECT_EQ(5, myops.mad(1, 2, 3))
})

TEST("basic.fib", fun()
{
    fun fib(n: int) {
        | 0 => 0
        | 1 => 1
        | _ => fib(n-1) + fib(n-2)
    }

    fun fib2(n: int) {
        fun fib2_(a: int, b: int, n: int) = if n <= 2 {a} else {fib2_(a+b, a, n-1)}
        fib2_(1, 1, n)
    }

    operator * (((a11, a12), (a21, a22)): (('t * 2) * 2),
            ((b11, b12), (b21, b22)): (('t * 2) * 2)) =
        ((a11*b11 + a12*b21, a11*b12 + a12*b22),
        (a21*b11 + a22*b21, a21*b12 + a22*b22))
    operator ** (a: (('t * 2) * 2), n: int)
    {
        val _0 = (0:>'t), _1 = (1:>'t)
        var p = ((_1, _0), (_0, _1))
        var a = a, n = n
        while n > 0 {
            if n % 2 == 0 {
                n /= 2
                a *= a
            }
            else {
                p *= a
                n -= 1
            }
        }
        p
    }

    fun fib3(n: int) {
        val a = ((1, 1), (1, 0))
        val a = a ** n
        a.1.0
    }

    fun start_fib_seq()
    {
        var a=1, b=1
        fun next_fib()
        {
            val t = b
            b = a
            a += t
            t
        }
        next_fib
    }
    val fib_seq = start_fib_seq()

    for x@i <- [: 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 :] {
        val fib_i = fib(i+1)
        val fib2_i = fib2(i+1)
        val fib3_i = fib3(i+1)
        val fib4_i = fib_seq()
        EXPECT_EQ(fib_i, x)
        EXPECT_EQ(fib2_i, x)
        EXPECT_EQ(fib3_i, x)
        EXPECT_EQ(fib4_i, x)
    }
})

exception BreakWith: int

TEST("basic.find", fun()
{
    fun find_iterative(a: 't [], f: 't->bool) {
        var i1 = 0, n = size(a)
        do
        {
            if f(a[i1]) {break}
            i1 += 1
        }
        while i1 < n
        if i1 < n {i1} else {-1}
    }

    fun find_fold(a: 't [], f: 't->bool) =
        find_opt(for i<-0:size(a) {f(a[i])}).value_or(-1)

    fun find_exn(a: 't [], f: 't -> bool): int
    {
        val n = size(a)
        try
        {
            for i <- 0:n {if f(a[i]) {throw BreakWith(i)}}
            -1
        }
        catch
        {
        | BreakWith(i) => i
        }
    }
    val a=[| 0, 1, 2, -10, 7, -3 |]
    fun is_negative(x: int) {x < 0}
    fun is_five(x: int) {x == 5}

    EXPECT_EQ(find_iterative(a, is_negative), 3)
    EXPECT_EQ(find_fold(a, is_negative), 3)
    EXPECT_EQ(find_exn(a, is_negative), 3)
    EXPECT_EQ(find_iterative(a, is_five), -1)
    EXPECT_EQ(find_fold(a, is_five), -1)
    EXPECT_EQ(find_exn(a, is_five), -1)
})

TEST("basic.make_empty_list", fun()
{
    fun make_null(a:'t):'t list = []

    val x = [: 1, 2, 3 :]
    EXPECT_EQ(make_null(x).length(), 0)
    EXPECT_EQ((x :: make_null(x)).length(), 1)
})

TEST("basic.math", fun()
{
    val c = {
       var a = 3.0
       var b = 4.0
       sqrt(a*a + b*b) + myops.div3(a*a + b*b)
    }

    EXPECT_NEAR(c, 13.3333333333, FLT_EPSILON*1.0)
    EXPECT_NEAR(atan(1.)*4, M_PI, DBL_EPSILON*10)
    EXPECT_NEAR(exp(-1.), 0.36787944117144233, DBL_EPSILON*10)
    val alpha = (M_PI/3 :> float)
    EXPECT_NEAR(sin(alpha), 0.8660254037844386f, FLT_EPSILON*10)
    EXPECT_NEAR(cos(alpha), 0.5f, FLT_EPSILON*10)
    EXPECT_EQ(myops.sqr(5), 25)
    EXPECT_NEAR(myops.sqr(0.1), 0.01, double(FLT_EPSILON))
})

TEST("basic.list.pattern_match", fun()
{
    fun f(l: int list) {
        | [] => (0, 0)
        | a :: [] => (a, 1)
        | a :: d :: [] => (a+d, 2)
        | _ => (-1, 56)
    }

    var lst : int list = []
    EXPECT_EQ(f(lst), (0, 0))

    lst = [: 1, :]
    EXPECT_EQ(f(lst), (1, 1))

    lst = [: 1, 2 :]
    EXPECT_EQ(f(lst), (3, 2))

    lst = [: 1, 2, 3 :]
    EXPECT_EQ(f(lst), (-1, 56))
})

TEST("basic.matrix.multiply", fun()
{
    val b = 4.1
    val m = array((10, 10), 2.)
    val n = array((10, 10), 3.)

    val mn = m[4:5, round(b):round(b)+1] * n[2:3, 3:4]

    EXPECT_EQ(size(mn), (1, 1))
    EXPECT_EQ(mn[0, 0], 6.0)
})

TEST("basic.function.try_catch", fun()
{
    fun foo(x:int) =
        try {
            fun bar(y:int) = if y >= 10 {throw Fail("bar")} else {x+y}
            fun baz(z:int) = bar(x+z)

            baz(5)
        } catch {
            | Fail _ => println ("Intentional exception occured; no worries"); -1
        }

    EXPECT_EQ(foo(1), 7)
    EXPECT_EQ(foo(5), -1)
})

TEST("basic.types.templates", fun()
{
    type ('a, 'b) template_fun_t = 'a -> 'b
    type 'a template_fun2_t = ('a, 'a) template_fun_t
    type ('a, 'b) tuple_signature_t = ('a, 'b, 'a template_fun2_t -> 'b template_fun2_t)
    type ('a, 'b) ct = A: ('a, 'a) template_fun_t | B: ('b, 'b) tuple_signature_t | C: ('a, 'b) ct

    // Check template_fun_t
    val my_func: (int, string) template_fun_t = fun (x:int) {string(x)}
    EXPECT_EQ(my_func(3), "3")

    // Check template_fun2_t
    val my_func2 = fun (x:int) {x*3}
    EXPECT_EQ(my_func2(3), 9)

    // Check tuple_signature_t
    val my_func3: double template_fun2_t = fun (x:double) {x*-3.}
    val strange_tuple: (int, double) tuple_signature_t = (
            3,
            3.25,
            (fun (f: int template_fun2_t) {my_func3} )
        )
    EXPECT_EQ(strange_tuple.0, 3)
    EXPECT_EQ(strange_tuple.1, 3.25)
    EXPECT_EQ(-9., strange_tuple.2(my_func2)(3.0))

    val ct_inst = C((A(cos) : (float, float) ct))
    EXPECT_EQ(match ct_inst {
        | C(A(f)) => f(0.f)
        | _ => -1.f
    }, 1.0f)
})

TEST("basic.record", fun()
{
    type 't point_t = {x:'t; y:'t}
    val p = point_t {x=5, y=6}
    fun mkpt(x: 't, y: 't) = point_t {x=x, y=y}

    EXPECT_EQ(f"{p}", "{x=5, y=6}")
    EXPECT_EQ(f"{mkpt(1, 0).{x=7}}", "{x=7, y=0}")
    EXPECT_EQ(f"{mkpt(1.5, 0.).{y=7.}}", "{x=1.5, y=7.0}")

    type 't rect_t = {x: 't; y: 't; width: 't; height: 't}

    fun contains(r: 'z rect_t, p: 'z point_t) =
        r.x <= p.x < r.x + r.width &&
        r.y <= p.y < r.y + r.height

    val ip = point_t {x=1, y=2}
    val fp = point_t {x=10.f, y=20.f}

    val ir = rect_t {x=0, y=0, width=10, height=10}
    val fr = rect_t {x=0.f, y=0.f, width=10.f, height=10.f}

    EXPECT_EQ(contains(ir, ip), true)
    EXPECT_EQ(contains(fr, fp), false)

    val vtx0 = [: (0, 0), (640, 0), (640, 480), (0, 480) :]
    val vtx1 = [: for (x, y) <- vtx0 {point_t {x=x*2, y=y*2}} :]
    val vtx2 = [: for {x, y} <- vtx1 {(x/2, y/2)} :]

    EXPECT_EQ(vtx2, vtx0)
})

TEST("basic.variant_with_record", fun()
{
    type tt = FF : {h: int} | Empty

    fun next(t: tt) {
        | FF {h} => FF {h=h+1}
        | _ => Empty
    }
    fun string(t: tt) {
        | FF {h} => f"FF({h})"
        | _ => "Empty"
    }

    val t = FF {h=0}
    EXPECT_EQ(string(next(t)), "FF(1)")

    type rr = {h1: int; h2: int}
    type tt2 = FF2 : rr | Empty2

    fun next(t: tt2) {
        | FF2 ({h1, h2}) => FF2 (rr {h1=h1+1, h2=h2-1})
        | _ => Empty2
    }
    fun string(t: tt2) {
        | FF2 ({h1, h2}) => f"FF2({h1}, {h2})"
        | _ => "Empty2"
    }

    val t = FF2 (rr {h1=1, h2=-1})
    EXPECT_EQ(string(next(t)), "FF2(2, -2)")

    type tt1 = FF1 : {h: int}
    fun next(t: tt1) {
        val FF1 {h} = t
        FF1 {h=h+1}
    }

    fun string(t: tt1) {
        val FF1 {h} = t
        f"FF1({h})"
    }

    val t = FF1 {h=100}
    EXPECT_EQ(string(next(t)), "FF1(101)")

    type tt15 = FF15 : {h: (int, int, int, bool, string)}
    fun next(t: tt15) {
        val FF15 {h=(h1, h2, h3, h4, h5)} = t
        FF15 {h=(h1+1, h2+2, h3+3, !h4, h5+"0")}
    }

    fun string(t: tt15) {
        | FF15 {h=(h1, h2, h3, h4, h5)} => f"FF15({h1}, {h2}, {h3}, {h4}, {repr(h5)})"
    }

    val t = FF15 {h=(10, 10, 10, false, "10")}
    EXPECT_EQ(string(next(t)), "FF15(11, 12, 13, true, \"100\")")
})

TEST("basic.ratio", fun()
{
    // two implementations of rational numbers using single-case variants
    // 1. tuple
    type ratio_t = Ratio: (int, int)

    operator + (Ratio(n1, d1): ratio_t, Ratio(n2, d2): ratio_t) {
        val n = n1*d2 + n2*d1
        val d = d1*d2
        val r = GCD(n, d)
        Ratio(n/r, d/r)
    }
    /*operator - (Ratio(n1, d1): ratio_t, Ratio(n2, d2): ratio_t) {
        val n = n1*d2 - n2*d1
        val d = d1*d2
        val r = GCD(n, d)
        Ratio(n/r, d/r)
    }
    operator * (Ratio(n1, d1): ratio_t, Ratio(n2, d2): ratio_t) {
        val n = n1*n2
        val d = d1*d2
        val r = GCD(n, d)
        Ratio(n/r, d/r)
    }
    operator / (Ratio(n1, d1): ratio_t, Ratio(n2, d2): ratio_t) {
        val n = n1*d2
        val d = d1*n2
        val r = GCD(n, d)
        Ratio(n/r, d/r)
    }*/

    fun string(Ratio(n, d): ratio_t) = f"{n}/{d}"

    val a = Ratio(33, 100), b = Ratio(85, 1000)
    EXPECT_EQ(string(a+b), "83/200")

    // 2. record
    type ratio2_t = Ratio2: {num: int; denom: int}

    // unfortunately, for now using record patterns directly
    // in function parameters is not yet supported
    /*operator + (a: ratio2_t, b: ratio2_t) {
        val n = a.num*b.denom + b.num*a.denom
        val d = a.denom*b.denom
        val r = GCD(n, d)
        Ratio2 {num=n/r, denom=d/r}
    }
    operator - (a: ratio2_t, b: ratio2_t) {
        val n = a.num*b.denom - b.num*a.denom
        val d = a.denom*b.denom
        val r = GCD(n, d)
        Ratio2 {num=n/r, denom=d/r}
    }
    operator * (a: ratio2_t, b: ratio2_t) {
        val n = a.num*b.num
        val d = a.denom*b.denom
        val r = GCD(n, d)
        Ratio2 {num=n/r, denom=d/r}
    }*/
    operator / (a: ratio2_t, b: ratio2_t) {
        val n = a.num*b.denom
        val d = a.denom*b.num
        val r = GCD(n, d)
        Ratio2 {num=n/r, denom=d/r}
    }

    fun string({num, denom}: ratio2_t) = f"{num}/{denom}"

    val a = Ratio2 {num=33, denom=100}, b = Ratio2 {num=85, denom=1000}
    EXPECT_EQ(string(a/b), "66/17")
})

TEST("basic.self_assignment", fun()
{
    type tree = Empty | Node: (int, tree, tree)

    var a = 5
    var b = ", ".join([: for i<-0:10 {string(i)} :])
    var c = [|[|[|1|], [|0|], [|0|]|], [|[|0|], [|1|], [|0|]|], [|[|0|], [|0|], [|1|]|]|]
    var c2 = [|[|[|1|], [|2|], [|3|]|], [|[|4|], [|5|], [|6|]|], [|[|7|], [|8|], [|9|]|]|]
    var d = ref [|1, 2, 3, 4, 5|]
    var e = (1, "abc", [|1, 2, 3|])
    var t = Node(5,
        Node(1,
            Node(-1, Empty, Empty),
            Node(3, Empty, Empty)),
        Node(10,
            Node(9, Empty, Empty),
            Node(1000, Empty, Empty)))
    operator == (a: tree, b: tree)
    {
        | (Empty, Empty) => true
        | (Node(va, a1, a2), Node(vb, b1, b2)) =>
            va == vb && a1 == b1 && a2 == b2
        | _ => false
    }
    fun string(t: tree) {
        | Empty => "Empty"
        | Node(v, l, r) => f"Node({v}, {l}, {r})"
    }

    a = a
    EXPECT_EQ(a, 5)
    b = b
    EXPECT_EQ(b, "0, 1, 2, 3, 4, 5, 6, 7, 8, 9")
    c = c
    EXPECT_EQ(c, [|[|[|1|], [|0|], [|0|]|], [|[|0|], [|1|], [|0|]|], [|[|0|], [|0|], [|1|]|]|])
    for i<-0:3 for j<-0:3 {c2[i][j] = c2[i][j]}
    EXPECT_EQ(c2, [|[|[|1|], [|2|], [|3|]|], [|[|4|], [|5|], [|6|]|], [|[|7|], [|8|], [|9|]|]|])
    d = d
    for i<-0:5 {(*d)[i] *= (*d)[i]}
    EXPECT_EQ(*d, [|1, 4, 9, 16, 25|])
    e.0 = e.0
    e.1 = e.1
    e.2 = e.2
    EXPECT_EQ(e, (1, "abc", [|1, 2, 3|]))
    t = t
    EXPECT_EQ(t, Node(5,
        Node(1,
            Node(-1, Empty, Empty),
            Node(3, Empty, Empty)),
        Node(10,
            Node(9, Empty, Empty),
            Node(1000, Empty, Empty))))
})

TEST("basic.ref", fun()
{
    val y = Some("abc")
    val u : string? ref = ref None

    *u = y
    EXPECT_EQ(u->value_or("0"), y.value_or("1"))

    val nested_ref: int ref ref ref = ref (ref (ref 5))
    EXPECT_EQ(***nested_ref, 5)

    ***nested_ref -= 5
    EXPECT_EQ(***nested_ref, 0)

    val tref = ref (1, 2, 3)
    EXPECT_EQ(tref->0 * tref->1 * tref->2, 6)
})

TEST("basic.option", fun()
{
    val x: int? = None
    val y = Some("abc")
    val z: string? = None

    EXPECT(x.isnone())
    EXPECT(z.isnone())
    EXPECT(y.issome())
    EXPECT_EQ(x.value_or(-1), -1)
    EXPECT_EQ(y.value_or(""), "abc")
    EXPECT_EQ(Some((1, 2, 3)).value_or((0, 0, 0)), (1, 2, 3))
})

TEST("basic.types.variant", fun()
{
    type type_t = Unit | Bool | Int | Float
                | Fun: ((type_t list), type_t)
                | Tuple: type_t list
                | Array: type_t
                | Var: type_t? ref

    fun tlist2str(args: type_t list) {
        | [] => ""
        | a :: [] => t2str(a)
        | a :: args1 => t2str(a) + ", " + tlist2str(args1)
    }

    fun t2str (typ: type_t) {
        | Unit => "void"
        | Bool => "bool"
        | Int => "int"
        | Float => "double"
        | Fun (args, rt) when args.length() != 1 => f"(({tlist2str(args)}) -> {t2str(rt)})"
        | Fun (args, rt) => f"({t2str(args.hd())} -> {t2str(rt)})"
        | Tuple(args) => f"({tlist2str(args)})"
        | Array(at) => f"{t2str(at)} []"
        | Var(x) => match *x { | Some(t1) => t2str(t1) | None => "<unknown>" }
    }

    EXPECT_EQ(
        f"({tlist2str(Unit :: Array(Int) :: Var(ref Some(Bool)) :: Tuple(Int::Float::[]) :: Fun([: Int, Int :], Int) :: [])})",
        "(void, int [], bool, (int, double), ((int, int) -> int))"
    )
    EXPECT_EQ(t2str(Var(ref None)), "<unknown>")
})

TEST("basic.list", fun()
{
    val l = 1 :: 2 :: 3 :: []
    val l2 = [: 1, 2, 3 :]
    EXPECT_EQ(l.length(), 3)
    EXPECT_THROWS(fun () {println(l.tl().tl().tl().tl())}, NullListError)
    EXPECT_EQ(l, l2)
    EXPECT_EQ(l.hd(), 1)
    EXPECT_EQ(l.tl(), 2 :: 3 :: [])
    EXPECT_EQ(l.hd() :: l.tl(), l)
    EXPECT_NE(l, 1 :: -1 :: 3 :: [])
    EXPECT_EQ(l <=> [: 1, 2, 3, 4 :], -1)
})

TEST("basic.list.reverse", fun()
{
    fun list_reverse(l: 't list): 't list
    {
        fun rev_(l_in: 't list, l_out: 't list): 't list =
            match l_in {
                | a :: rest => rev_(rest, (a :: l_out))
                | [] => l_out
            }
        rev_(l, [])
    }

    val l = 1 :: 2 :: 3 :: []
    EXPECT_EQ(list_reverse(l), [: 3, 2, 1 :])
    EXPECT_EQ((-1 :: l).rev(), [: 3, 2, 1, -1 :])
})

TEST("basic.list.map", fun()
{
    fun list_map(l: 'a list, f: 'a -> 'b): 'b list
    {
        fun map_(l_in: 'a list, l_out: 'b list): 'b list =
            match l_in {
                | a :: rest => map_(rest, (f(a) :: l_out))
                | [] => l_out
            }
        map_(l, []).rev()
    }

    val strings = list_map((1 :: 2 :: 3 :: []), (string: int->string))
    EXPECT_EQ(strings, [: "1", "2", "3" :])

    val cosines : double list = list_map((1. :: 2. :: 3. :: []), cos)
    val expected = [: 0.5403023058681398, -0.4161468365471424, -0.9899924966004454 :]
    EXPECT_NEAR(cosines, expected, DBL_EPSILON*10)
})

TEST("basic.list.zip", fun()
{
    fun list_zip(la: 'a list, lb: 'b list): ('a, 'b) list
    {
        fun zip_(_: 'a list, _: 'b list, _: ('a, 'b) list) {
            | (a :: rest_a, b :: rest_b, lab_out) => zip_(rest_a, rest_b, (a, b) :: lab_out)
            | (_, _, lab_out) => lab_out.rev()
        }

        zip_(la, lb, [])
    }

    val zipped = list_zip((1 :: 2 :: 3 :: []), ("a" :: "b" :: "c" :: []))
    EXPECT_EQ(zipped.length(), 3)
    EXPECT_EQ(zipped.tl().tl().hd(), (3, "c"))

    val triples = [: for c <- "abcdef", i <- [: 1, 2, 3, 4, 5, 6 :] {(i, i*i, c)} :]
    EXPECT_EQ(triples, [: (1, 1, 'a'), (2, 4, 'b'), (3, 9, 'c'), (4, 16, 'd'), (5, 25, 'e'), (6, 36, 'f') :])
})

TEST("basic.list.unzip", fun()
{
    fun list_unzip(lab: ('a, 'b) list): ('a list, 'b list)
    {
        fun unzip_(lab_in: ('a, 'b) list, la_out: 'a list, lb_out: 'b list): ('a list, 'b list) =
            match lab_in {
                | (a, b) :: rest_ab => unzip_(rest_ab, a :: la_out, b :: lb_out)
                | _ => (la_out.rev(), lb_out.rev())
            }

        unzip_(lab, [], [])
    }

    val ll = ("a", 1) :: ("b", 2) :: ("c", 3) :: []
    val unzipped = list_unzip(ll)
    EXPECT_EQ(unzipped.0, [: "a", "b", "c" :])
    EXPECT_EQ(unzipped.1, [: 1, 2, 3 :])
    val unzipped2 = [: @unzip for si <- ll {si} :]
    EXPECT_EQ(unzipped2.0, [: "a", "b", "c" :])
    EXPECT_EQ(unzipped2.1, [: 1, 2, 3 :])
})

TEST("basic.list.sort", fun()
{
    EXPECT_EQ([: 10, 355, 113, -1, 2, 26, 1, 1949, 0, 299792458,
        -460, 451, -11034, 8848 :].sort((<)),
        [: -11034, -460, -1, 0, 1, 2, 10, 26, 113, 355, 451, 1949, 8848, 299792458 :])
})

TEST("basic.myops", fun()
{
    val x = myops.add_scaled([|1.f, 2.f, 3.f|], [|4.f, 5.f, 6.f|], 0.1f)[0]
    EXPECT_NEAR(x, 1.4f, FLT_EPSILON*10)
    EXPECT_EQ(myops.sum_arr([| for i <- 0:100 {i*i} |]), 328350)
})

TEST("basic.array.compose", fun()
{
    val m0 = [|1, 2, 3|]
    val m1 = [|\m0, 4; 0, \m0|]

    EXPECT_EQ(m1, [| 1, 2, 3, 4; 0, 1, 2, 3 |])

    val eye22 = [| 1., 0.;
                   0., 1. |]

    val m1 = [| \(eye22 + eye22), \(eye22 - eye22);
                \(eye22 * 4.),    \(eye22 * 5.) |]
    val expected1 = [| 2., 0., 0., 0.;
                       0., 2., 0., 0.;
                       4., 0., 5., 0.;
                       0., 4., 0., 5. |]
    EXPECT_EQ(m1, expected1)

    val m2 = [| eye22 .+ eye22, eye22 .- eye22;
                eye22 * 4.,    eye22 * 5. |]
    val expected2 = [| [| 2., 0.; 0., 2.|], [| 0., 0.; 0., 0.|];
                       [| 4., 0.; 0., 4.|], [| 5., 0.; 0., 5.|] |]
    EXPECT_EQ(m2, expected2)
})

TEST("basic.loop.squares", fun()
{
    fun gen_squares(n: int)
    {
        var squares: int list = []
        var i = 0
        while i < n {
            squares = i*i :: squares
            i += 1
        }
        squares.rev()
    }

    EXPECT_EQ(gen_squares(4), [: 0, 1, 4, 9 :])
})

TEST("basic.overloaded", fun()
{
    val t = (1, "abc") + (0.125, "def")
    EXPECT_EQ(t, (1.125, "abcdef"))
    type 't point_t = {x: 't; y: 't}

    operator + (p1: 't point_t, p2: 't point_t) =
        point_t { x=p1.x + p2.x, y=p1.y + p2.y }

    EXPECT_EQ((point_t {x=1, y=2}) + (point_t {x=0, y=100}), point_t {x=1, y=102})
})

TEST("basic.types.conversions", fun()
{
    EXPECT_EQ(3.f, float(1) + 2)

    val my_pi = "3.14".to_double().value_or(0.0)
    EXPECT_NEAR(my_pi * 2, 6.28, 1e-10)
})

TEST("basic.array.init_u", fun()
{
    val a = array((3, 4), 0.)
    val (m, n) = size(a)
    for i <- 0:m for j <- 0:n {
        if j >= i {a[i,j] = 1.}
    }
    EXPECT_EQ(a, [| 1., 1., 1., 1.;
                    0., 1., 1., 1.;
                    0., 0., 1., 1. |])
})

TEST("basic.assert", fun()
{
    val k = -1
    EXPECT_NO_THROWS(fun () { assert (0 == k-k) }, msg="assert(0=0)")
    EXPECT_THROWS(fun () { assert (1 == k-k) }, AssertError)
})

/*TEST("basic.stack_overflow", fun()
{
    val rng = new_uniform_rng(0xffffffffUL)
    fun foo(n:int) { if rng(0, 10) > 100 {n} else {2*foo(n-1)} }
    EXPECT_THROWS(fun () {ignore(foo(1000))}, StackOverflowError)
})*/

TEST("basic.string", fun()
{
    EXPECT_EQ("yellow", "y" + "hello"[1:5] + "w")
    EXPECT_EQ("yellow"[:.-1], "yello")
    EXPECT_EQ(f"abc{2*2}def", "abc4def")
    EXPECT_EQ(f"abc{{2*2}}def", "abc{2*2}def")
    EXPECT_EQ(r"Dear (\w+),.+Best regards,\n(\w+)", "Dear (\\w+),.+Best regards,\\n(\\w+)")

    EXPECT_EQ(", ".join([: "a", "b", "c" :]), "a, b, c")

    val str = "This is a sentence made of words separated by spaces."
    EXPECT_EQ(str.tokens(fun (c) {c == ' '}),
        [:"This", "is", "a", "sentence", "made", "of", "words", "separated", "by", "spaces." :])

    EXPECT_EQ("Привет! 你好吗?".length(), 12)
})

TEST("basic.templates.variants", fun()
{
    type 't tree_t = Node: { balance:int; left:'t tree_t; right:'t tree_t } | Leaf: 't
    val node_list = Leaf(3) :: Leaf(5) :: (Node { balance=0, left=Leaf(1), right=Leaf(0) }) ::
        (Node { balance=-1, left=Leaf(1), right=Node {balance=0, left=Leaf(10), right=Leaf(100)}}) ::
        []

    fun depth(t: 'x tree_t) {
        | Leaf _ => 0
        | Node {balance, left, right} => 1 + max(depth(left), depth(right))
    }

    val depth_list = [: for n <- node_list {depth(n)} :]
    EXPECT_EQ(depth_list, [: 0, 0, 1, 2 :])
})

TEST("basic.keyword_args", fun()
{
    fun sqrt(a: 't, ~n: int=2, ~use_abs:bool=false)
    {
        if n == 2 {Math.sqrt(a)}
        else {
            if n % 2 == 0 || use_abs {
                pow(abs(a), (1./n :> 't))
            } else {
                val (a, s) = if a < (0.:>'t) {(-a, -1)} else {(a, 1)}
                s*pow(a, (1./n :> 't))
            }
        }
    }

    EXPECT_NEAR(sqrt(81.0), 9.0, 1e-10)
    EXPECT_NEAR(sqrt(-81.0, use_abs=true, n=4), 3.0, 1e-10)
    EXPECT_NEAR(sqrt(-27.f, n=3), -3.f, 1e-6f)
})

TEST("basic.finally", fun()
{
    var finalized0 = "", finalized1 = ""
    val res0 =
        try {
            var num = 1, denom = 0
            num/denom
        }
        catch {
            | DivByZeroError => 0
        }
        finally {
            finalized0 = "ok0"
        }
    val res1 =
        try {
            var num = 1, denom = 1
            num/denom
        }
        catch {
            | DivByZeroError => 0
        }
        finally {
            finalized1 = "ok1"
        }
    EXPECT_EQ(res0, 0)
    EXPECT_EQ(finalized0, "ok0")
    EXPECT_EQ(res1, 1)
    EXPECT_EQ(finalized1, "ok1")
})