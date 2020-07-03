/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtins import *" directive in the beginning */

exception Fail: string
exception NotFoundError
exception OutOfRangeError
exception NoMatchError
exception OptionError
exception NullListError
exception SizeMismatchError
exception FileOpenError
exception NullFileError
exception IOError
exception AssertError
exception Break

fun assert(f: bool) = if !f {throw AssertError}

fun ignore(_: 't) {}

operator != (a: 't, b: 't): bool = !(a == b)
operator > (a: 't, b: 't): bool = b < a
operator >= (a: 't, b: 't): bool = !(a < b)
operator <= (a: 't, b: 't): bool = !(b < a)

// 't?, int? etc. can be used instead of 't option, int option ...
type 't option = None | Some: 't

fun getOpt(x: 't?, defval: 't) = match x { | Some(x) => x | _ => defval }

pure nothrow fun length(s: string): int = ccode "return s->length;"
pure fun join(sep: string, strs:string []): string = ccode
    "return fx_strjoin(0, 0, sep, (fx_str_t*)strs->data, strs->dim[0].size, fx_result);"
pure fun join_embrace(begin: string, end: string, sep: string, strs:string []): string = ccode
    "return fx_strjoin(begin, end, sep, (fx_str_t*)strs->data, strs->dim[0].size, fx_result);"

fun join(sep: string, strs: string list) =
    join(sep, [for s <- strs {s}])

operator + (a: string, b: string): string = ccode "fx_str_t s[] = {*a, *b}; return fx_strjoin(0, 0, 0, s, 2, fx_result);"
operator + (a: string, b: char): string = ccode "
    fx_str_t s[] = {*a, {0, &b, 1}};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);"
operator + (a: char, b: string): string = ccode "
    fx_str_t s[] = {{0, &a, 1}, *b};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);"
operator + (a: char, b: char): string = ccode "
    char_ cc[] = {a, b};
    return fx_make_str(cc, 2, fx_result);"
operator + (l1: 't list, l2: 't list) {
    nothrow fun link2(l1: 't list, l2: 't list): 't list = ccode "fx_link_lists(l1, l2, fx_result);"
    match (l1, l2) {
        | ([], _) => l2
        | (_, []) => l1
        | _ => link2([: for x <- l1 {x} :], l2)
    }
}

fun string(a: bool) = if a {"true"} else {"false"}
pure fun string(a: int): string = ccode "return fx_itoa(a, fx_result);"
pure fun string(c: char): string = ccode "return fx_make_str(&c, 1, fx_result);"
pure fun string(a: float): string = ccode "char buf[32]; sprintf(buf, (a == (int)a ? \"%.1f\" : \"%.8g\"), a); return fx_ascii2str(buf, -1, fx_result);"
pure fun string(a: double): string = ccode "char buf[32]; sprintf(buf, (a == (int)a ? \"%.1f\" : \"%.16g\"), a); return fx_ascii2str(buf, -1, fx_result);"
fun string(a: string) = a
fun ord(c: char) = (c :> int)
fun chr(i: int) = (i :> char)

fun repr(a: 't): string = string(a)
fun repr(a: string) = "\"" + a + "\""
fun repr(a: char) = "'" + a + "'"

fun string((a, b): ('a, 'b)) = "(" + repr(a) + ", " + repr(b) + ")"
fun string((a, b, c): ('a, 'b, 'c)) = "(" + repr(a) + ", " + repr(b) + ", " + repr(c) + ")"
fun string((a, b, c, d): ('a, 'b, 'c, 'd)) =
    "(" + repr(a) + ", " + repr(b) + ", " + repr(c) + repr(d) + ")"
fun string(a: 't ref) = "ref(" + repr(*a) + ")"

fun string(a: 't [])
{
    join_embrace("[", "]", ", ", [for x <- a {repr(x)}])
}

fun string(l: 't list)
{
    join_embrace("[", "]", ", ", [for x <- l {repr(x)}])
}
pure fun string(a: char []): string = ccode "return fx_make_str((char_*)a->data, a->dim[0].size, fx_result);"

pure nothrow operator == (a: string, b: string): bool = ccode
    "
    return (bool)(a->length == b->length &&
            (a->length == 0 ||
            memcmp(a->data, b->data, a->length*sizeof(a->data[0])) == 0));
    "

// [TODO] implement more clever string comparison operation
pure nothrow operator < (a: string, b: string): bool = ccode
    "
    int_ alen = a->length, blen = b->length;
    bool ashorter = alen < blen;
    int_ minlen = ashorter ? alen : blen;
    const char_ *adata = a->data, *bdata = b->data;
    for(int_ i = 0; i < minlen; i++) {
        int_ ai = (int_)adata[i], bi = (int_)bdata[i], diff = ai - bi;
        if(diff != 0)
            return diff < 0;
    }
    return ashorter;
    "

// compare the pointers, not the content. Maybe need a separate operator for that.
pure nothrow operator == (a: 't ref, b: 't ref): bool = ccode
    "return a == b;"

nothrow fun atoi(a: string): int? = ccode
    "
    bool ok = true;
    fx_atoi(a, &fx_result->u.Some, &ok, 10);
    fx_result->tag = (int)ok;
    "

pure nothrow fun sat_uint8(i: int): uint8 = ccode "
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_uint8(f: float): uint8 = ccode "
    int i = fx_roundf2i(f);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_uint8(d: double): uint8 = ccode "
    int_ i = fx_round2I(d);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_int8(i: int): int8 = ccode "
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_int8(f: float): int8 = ccode "
    int i = fx_roundf2i(f);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_int8(d: double): int8 = ccode "
    int_ i = fx_round2I(d);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_uint16(i: int): uint16 = ccode "
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_uint16(f: float): uint16 = ccode "
    int i = fx_roundf2i(f);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_uint16(d: double): uint16 = ccode "
    int_ i = fx_round2I(d);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_int16(i: int): int16 = ccode "
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

pure nothrow fun sat_int16(f: float): int16 = ccode "
    int i = fx_roundf2i(f);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

pure nothrow fun sat_int16(d: double): int16 = ccode "
    int_ i = fx_round2I(d);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

// do not use lrint(x), since it's slow. and (int)round(x) is even slower
pure nothrow fun round(x: float): int = ccode "return fx_roundf2I(x);"
pure nothrow fun round(x: double): int = ccode "return fx_round2I(x);"

fun min(a: 't, b: 't) = if a <= b {a} else {b}
fun max(a: 't, b: 't) = if a >= b {a} else {b}
fun abs(a: 't) = if a >= (0 :> 't) {a} else {-a}
fun clip(x: 't, a: 't, b: 't) = if a <= x < b {x} else if x < a {a} else {b}

nothrow fun print_string(a: string): void = ccode "fx_fputs(stdout, a);"

fun print(a: 't) = print_string(string(a))
fun print_repr(a: 't) = print(a)
nothrow fun print(a: int): void = ccode "printf(\"%zd\", a);"
nothrow fun print(a: float): void = ccode "printf((a == (int)a ? \"%.1f\" : \"%.8g\"), a);"
nothrow fun print(a: double): void = ccode "printf((a == (int)a ? \"%.1f\" : \"%.16g\"), a);"
fun print(a: string) = print_string(a)
fun print_repr(a: string) { print("\""); print(a); print("\"") }
fun print(a: 't [])
{
    print("[")
    for i <- 0:, x <- a {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}
nothrow fun print(a: char []): void = ccode
    "
    fx_str_t str = {0, (char_*)a->data, a->dim[0].size};
    fx_fputs(stdout, &str);
    "

fun print(a: 't [,])
{
    print("[")
    val (m, n) = size(a)
    for i <- 0:m {
        for j <- 0:n {
            if j > 0 {print(", ")}
            print_repr(a[i,j])
        }
        if i < m-1 {print(";\n ")} else {print("]\n")}
    }
}
fun print(l: 't list)
{
    print("[")
    for i <- 0:, x <- l {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}
fun print((a, b): ('a, 'b))
{
    print("("); print_repr(a); print(", ");
    print_repr(b); print(")")
}
fun print((a, b, c): ('a, 'b, 'c))
{
    print("("); print_repr(a); print(", ");
    print_repr(b); print(", "); print_repr(c); print(")")
}
fun print((a, b, c, d): ('a, 'b, 'c, 'd))
{
    print("("); print_repr(a); print(", ");
    print_repr(b); print(", "); print_repr(c);
    print(", "); print_repr(d); print(")")
}
fun print(a: 't ref) {
    print("ref("); print_repr(*a); print(")")
}

fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }

fun array(n: int, x: 't) = [for i <- 0:n {x}]
fun array((m: int, n: int), x: 't) = [for i <- 0:m for j <- 0:n {x}]
fun array((m: int, n: int, l: int), x: 't) = [for i <- 0:m for j <- 0:n for k <- 0:l {x}]

pure nothrow fun size(a: 't []): int = ccode "return a->dim[0].size;"
pure nothrow fun size(a: 't [,]): (int, int) = ccode
    "
    fx_result->t0=a->dim[0].size;
    fx_result->t1=a->dim[1].size;
    "
pure nothrow fun size(a: 't [,,]): (int, int, int) = ccode
    "
    fx_result->t0=a->dim[0].size;
    fx_result->t1=a->dim[1].size;
    fx_result->t2=a->dim[2].size;
    "
