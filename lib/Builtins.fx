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
exception ListSizeMismatchError

fun ignore(_: 't) {}

type 't option = None | Some: 't

fun getOpt(x: 't option, defval: 't) = match (x) { | Some(x) => x | _ => defval }

pure nothrow fun length(s: string): int = ccode "return s->length;"
fun string(a: bool) = (a :> string)
fun string(a: int) = (a :> string)
fun string(a: float) = (a :> string)
fun string(a: double) = (a :> string)
fun string(a: string) = a

operator == (a: string, b: string): bool = ccode "
    *fx_result = (bool)(a->length == b->length && (a->length == 0 || memcmp(a->data, b->data, a->length*sizeof(a->data[0])) == 0));
    return FX_OK;"
/*fun string(a: int): string = ccode "char buf[32]; sprintf(buf, \"%d\", a); return fx_cstr2str(buf, -1, fx_result);"
fun string(a: float): string = ccode "char buf[32]; sprintf(buf, \"%.10g\", a); return fx_cstr2str(buf, -1, fx_result);"
fun string(a: double): string = ccode "char buf[32]; sprintf(buf, \"%.20g\", a); return fx_cstr2str(buf, -1, fx_result);"
operator + (a: string, b: string): string = ccode "fx_str_t* s[] = {a, b}; return fx_strjoin(0, s, 2, fx_result);"
operator + (a: string, b: char): string = ccode "
    fx_str_t bstr = {0, &b, 1}, *s[] = {a, &bstr};
    return fx_strjoin(0, s, 2, fx_result);"
operator + (a: char, b: string): string = ccode "
    fx_str_t astr = {0, &a, 1}, *s[] = {&astr, b};
    return fx_strjoin(0, s, 2, fx_result);"*/

fun atoi(a: string): int option
{
    fun atoi_(a: string): (int, bool) = ccode
        "return fx_atoi(a, &fx_result->t0, &fx_result->t1, 10);"
    match (atoi_(a)) {
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

pure nothrow fun round(x: float): int = ccode "return (int_)lrintf(x);"
pure nothrow fun round(x: double): int = ccode "return (int_)lrint(x);"

fun min(a: 't, b: 't) = if (a <= b) a else b
fun max(a: 't, b: 't) = if (a >= b) a else b
fun abs(a: 't) = if (a >= (0 :> 't)) a else -a

fun print_string(a: string): void = ccode "return fx_puts(a->data);"

fun print(a: 't) = print_string(string(a))
fun print(a: string) = print_string(a)
fun print(l: 't list)
{
    print("[")
    for (i in 0:, x in l) {
        if (i > 0) print(", ")
        print (x)
    }
    print("]")
}

fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }

fun array(n: int, x: 't) = [for (i in 0:n) x]
fun array((m: int, n: int), x: 't) = [for (i in 0:m) for (j in 0:n) x]
fun array((m: int, n: int, l: int), x: 't) = [for (i in 0:m) for (j in 0:n) for (k in 0:l) x]

pure fun size(a: 't []): int = ccode "*fx_result = a->dim[0]; return FX_OK;"
pure fun size(a: 't [,]): (int, int) = ccode
    "fx_result->t0=a->dim[0].size; fx_result->t1=a->dim[1].size; return FX_OK;"
pure fun size(a: 't [,,]): (int, int, int) = ccode
    "fx_result->t0=a->dim[0].size;
    fx_result->t1=a->dim[1].size;
    fx_result->t2=a->dim[2].size;
    return FX_OK;"
