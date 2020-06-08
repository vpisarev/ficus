/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtins import *" directive in the beginning */

exception Failure: string
exception NotFoundError
exception IndexError
exception NoMatchError
exception OptionError
exception NullListError
exception SizeMismatchError
exception FileOpenError
exception NullFileError
exception IOError
exception AssertError

fun assert(f: bool) = if !f {throw AssertError}

fun ignore(_: 't) {}

// 't?, int? etc. can be used instead of 't option, int option ...
type 't option = None | Some: 't

fun getOpt(x: 't?, defval: 't) = match x { | Some(x) => x | _ => defval }

pure nothrow fun length(s: string): int = ccode "return s->length;"
pure fun join(sep: string, strs:string []): string = ccode
    "return fx_strjoin(sep, (fx_str_t*)strs->data, strs->dim[0].size, fx_result);"

fun join(sep: string, strs: string list) =
    join(sep, [for s <- strs {s}])

fun string(a: bool): string = if a {"true"} else {"false"}
fun string(a: int): string = ccode "char buf[32]; sprintf(buf, \"%d\", a); return fx_cstr2str(buf, -1, fx_result);"
fun string(a: float): string = ccode "char buf[32]; sprintf(buf, \"%.10g\", a); return fx_cstr2str(buf, -1, fx_result);"
fun string(a: double): string = ccode "char buf[32]; sprintf(buf, \"%.20g\", a); return fx_cstr2str(buf, -1, fx_result);"
fun string(a: string) = a
fun string(a: 't []) {
    val n = size(a)
    join(", ",
        [for i <- 0:n {
            val ai = string(a[i])
            if i == 0 {"["+ai} else if i < n-1 {ai} else {ai+"]"}
        }])
}

operator != (a: 't, b: 't): bool = !(a == b)

pure nothrow operator == (a: string, b: string): bool = ccode
    "
    return (bool)(a->length == b->length &&
            (a->length == 0 ||
            memcmp(a->data, b->data, a->length*sizeof(a->data[0])) == 0));
    "
pure nothrow operator == (a: 't ref, b: 't ref): bool = ccode
    "return a == b;"

operator + (a: string, b: string): string = ccode "fx_str_t s[] = {*a, *b}; return fx_strjoin(0, s, 2, fx_result);"
operator + (a: string, b: char): string = ccode "
    fx_str_t s[] = {*a, {0, &b, 1}};
    return fx_strjoin(0, s, 2, fx_result);"
operator + (a: char, b: string): string = ccode "
    fx_str_t s[] = {{0, &a, 1}, *b};
    return fx_strjoin(0, s, 2, fx_result);"

fun atoi(a: string): int?
{
    fun atoi_(a: string): (int, bool) = ccode
        "return fx_atoi(a, &fx_result->t0, &fx_result->t1, 10);"
    match atoi_(a) {
    | (x, true) => Some(x)
    | _ => None
    }
}

pure nothrow fun sat_uint8(i: int): uint8 = ccode "
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_uint8(f: float): uint8 = ccode "
    int i = (int)(f < 0 ? f - 0.5f : f + 0.5f);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_uint8(d: double): uint8 = ccode "
    int i = (int)(d < 0 ? d - 0.5 : d + 0.5);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);"

pure nothrow fun sat_int8(i: int): int8 = ccode "
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_int8(f: float): int8 = ccode "
    int i = (int)(f < 0 ? f - 0.5f : f + 0.5f);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_int8(d: double): int8 = ccode "
    int i = (int)(d < 0 ? d - 0.5 : d + 0.5);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);"

pure nothrow fun sat_uint16(i: int): uint16 = ccode "
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_uint16(f: float): uint16 = ccode "
    int i = (int)(f < 0 ? f - 0.5f : f + 0.5f);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_uint16(d: double): uint16 = ccode "
    int i = (int)(d < 0 ? d - 0.5 : d + 0.5);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);"

pure nothrow fun sat_int16(i: int): int16 = ccode "
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

pure nothrow fun sat_int16(f: float): int16 = ccode "
    int i = (int)(f < 0 ? f - 0.5f : f + 0.5f);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

pure nothrow fun sat_int16(d: double): int16 = ccode "
    int i = (int)(d < 0 ? d - 0.5 : d + 0.5);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);"

pure nothrow fun round(x: float): int = ccode "return (int_)lrintf(x);"
pure nothrow fun round(x: double): int = ccode "return (int_)lrint(x);"

fun min(a: 't, b: 't) = if a <= b {a} else {b}
fun max(a: 't, b: 't) = if a >= b {a} else {b}
fun abs(a: 't) = if a >= (0 :> 't) {a} else {-a}
fun clip(x: 't, a: 't, b: 't) = if a <= x < b {x} else if x < a {a} else {b}

fun print_string(a: string): void = ccode "return fx_puts(stdout, a->data);"

fun print(a: 't) = print_string(string(a))
nothrow fun print(a: int): void = ccode "printf(\"%zd\", a);"
nothrow fun print(a: float): void = ccode "printf(\"%.8g\", a);"
nothrow fun print(a: double): void = ccode "printf(\"%.16g\", a);"
fun print(a: string) = print_string(a)
fun print(l: 't list)
{
    print("[")
    for i <- 0:, x <- l {
        if i > 0 {print(", ")}
        print(x)
    }
    print("]")
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
