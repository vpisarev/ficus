/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some basic tests

from UTest import *
import myops
import Args
import Math

val FLT_EPSILON = 1e-6f
val DBL_EPSILON = 1e-12

TEST("basic.match.default_pattern", fun()
{
    fun foo(n: int) {
        | 1 => 1
        | 2 => 2
        | 3 => 3
        | _ => 0
    }

    var n = 5
    EXPECT_EQ(foo(n), 0)

    n = 2
    EXPECT_EQ(foo(n), 2)
})

TEST("basic.myops.mad_ccode", fun()
{
    EXPECT_EQ(5, myops.mad(1, 2, 3))
})

TEST("basic.fib_10", fun()
{
    fun fib(n:int) = if n == 0 {1} else if n == 1 {1} else {fib(n-1) + fib(n-2)}
    EXPECT_EQ(fib(10), 89)
})

TEST("basic.fib.match", fun()
{
    fun fib2(n:int) {
        | 0 => 1
        | 1 => 1
        | _ => fib2(n-1) + fib2(n-2)
    }

    for x@i <- [: 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 :] {
        EXPECT_EQ(fib2(i), x)
    }
})

/*TEST("basic.make_empty_list", fun()
{
    fun make_null(a:'t):'t list = []

    val x = [: 1, 2, 3 :]
    EXPECT_EQ(make_null(x).length(), 0)
    EXPECT_EQ((x :: make_null(x)).length(), 1)
})*/

TEST("basic.math", fun()
{
    val c = {
       var a = 3.0
       var b = 4.0
       Math.sqrt(a*a + b*b) + myops.div3(a*a + b*b)
    }

    EXPECT_NEAR(c, 13.3333333333, FLT_EPSILON*1.0)
    EXPECT_NEAR(Math.atan(1.)*4, 1., DBL_EPSILON*10)
    EXPECT_NEAR(Math.exp(-1.), 0.36787944117144233, DBL_EPSILON*10)
    val alpha = (Math.pi/3 :> float)
    EXPECT_NEAR(Math.sin(alpha), 0.8660254037844386f, FLT_EPSILON*10)
    EXPECT_NEAR(Math.cos(alpha), 0.5f, FLT_EPSILON*10)
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
            fun bar(y:int) = if y >= 5 {throw Fail("bar")} else {x+y}
            fun baz(z:int) = bar(z)+x

            baz(5)
        } catch {
            | Fail _ => println ("Intentional exception occured"); -1
        }

    EXPECT_EQ(foo(1), 7)
    EXPECT_EQ(foo(5), -1)
})

/*TEST("basic.types.templates", fun()
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
    val my_func3: (double) template_fun2_t = fun (x:double) {x*-3.}
    val strange_tuple: (int, double) tuple_signature_t = (
            3,
            3.25,
            (fun (f: (int) template_fun2_t) {my_func3} )
        )
    EXPECT_EQ(strange_tuple.0, 3)
    EXPECT_EQ(strange_tuple.1, 3.25)
    EXPECT_EQ(-9., strange_tuple.2(my_func2)(3.0))

    val cosf = (Math.cos: float->float)
    val ct_inst = C((A(cosf) : (float, float) ct))
    EXPECT_EQ(match ct_inst {
        | C(A(f)) => f(0.f)
        | _ => -1.f
    }, 1.0f)
})*/

/*TEST("basic.point_struct", fun()
{
    type 't point_t = {x:'t, y:'t}
    val p = point_t {x=5, y=6}
    fun string(p: 't point_t): string = f"p=({p.x}, {p.y})"
    fun mkpt(x: int, y:int) = point_t {x=x, y=y}

    EXPECT_EQ(string(p), "p=(5, 6)")
    EXPECT_EQ(string(mkpt(1, 0).{x=7}), "p=(7, 0)")
    EXPECT_EQ(string(mkpt(1, 0).{y=7}), "p=(1, 7)")
})*/

/*TEST("basic.ref", fun()
{
    val y = Some("abc")
    val u = ref (None : string?)

    *u = y
    EXPECT_EQ(getOpt(*u, "0"), getOpt(y, "1"))

    val nested_ref: int ref ref ref = ref (ref (ref 5))
    EXPECT_EQ(***nested_ref, 5)

    ***nested_ref -= 5
    EXPECT_EQ(***nested_ref, 0)
})

TEST("basic.option", fun()
{
    val x: int? = (None :> int?)
    val y = Some("abc")
    val z: string? = None

    EXPECT(isNone(x))
    EXPECT(isNone(z))
    EXPECT(isSome(y))
    EXPECT_EQ(getOpt(x, -1), -1)
    EXPECT_EQ(getOpt(y, ""), "abc")
    EXPECT_EQ(2, getOpt(Some((1, 2, 3)), (0, 0, 0)).1)
})*/

/*TEST("basic.types.variant", fun()
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
        f"({tlist2str(Unit :: Array(Int) :: Var(ref Some(Bool)) :: Fun([: Int, Int :], Int) :: [])})",
        "(void, int [], bool, ((int, int) -> int))"
    )
    EXPECT_EQ(t2str(Var(ref None)), "<unknown>")
})*/

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

    val cosines = list_map((1. :: 2. :: 3. :: []), (Math.cos: double->double))
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

        zip_(la, lb, []).rev()
    }

    val zipped = list_zip((1 :: 2 :: 3 :: []), ("a" :: "b" :: "c" :: []))
    EXPECT_EQ(zipped.length(), 3)
    EXPECT_EQ(zipped.tl().tl().hd(), (3, "c"))
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

    val unzip = list_unzip(("a", 1) :: ("b", 2) :: ("c", 3) :: [])
    EXPECT_EQ(List.length(unzip.1), 3)
})

TEST("basic.myops", fun()
{
    val x = myops.add_scaled([1.f, 2.f, 3.f], [4.f, 5.f, 6.f], 0.1f)[0]
    EXPECT_NEAR(x, 1.4f, FLT_EPSILON*10)
    EXPECT_EQ(myops.sum_arr([for i <- 0:100 {i*i}]), 328350)
})

TEST("basic.array.compose", fun()
{
    val m0 = [1, 2, 3]
    val m1 = [\m0, 4; 0, \m0]

    EXPECT_EQ(m1, [1, 2, 3, 4; 0, 1, 2, 3])
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

/*TEST("basic.overloaded", fun()
{
    val t = (1, "abc") + (0.125, "def")
    EXPECT_EQ(t, (1.125, "abcdef"))
    type 't point_t = {x: 't, y: 't}

    operator + (p1: 't point_t, p2: 't point_t) =
        point_t { x=p1.x + p2.x, y=p1.y + p2.y }

    EXPECT_EQ((point_t {x=1, y=2}) + (point_t {x=0, y=100}), point_t {x=1, y=102})
})*/

TEST("basic.types.conversions", fun()
{
    EXPECT_EQ(3.f, float(1) + 2)

    val my_pi = getOpt(atoi("314"), 0)*0.01f
    EXPECT_NEAR(6.28f, my_pi * 2, 1e-6f)
})

val (run, options) = test_parse_options(Args.arguments())
if run {test_run_all(options)}
