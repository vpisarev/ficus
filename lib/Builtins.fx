/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtins import *" directive in the beginning */

val __ficus_major__ : int = @ccode { FX_VERSION_MAJOR }
val __ficus_minor__ : int = @ccode { FX_VERSION_MINOR }
val __ficus_patchlevel__ : int = @ccode { FX_VERSION_PATCH }
val __ficus_suffix__ : string = @ccode { FX_MAKE_STR(FX_VERSION_SUFFIX)}
val __ficus_git_commit__ : string = @ccode { FX_MAKE_STR(FX_GIT_COMMIT) }

// __ficus_version__, as a tuple, can be easily compared with a specific version, e.g.
// if __ficus_version__ >= (1, 0, 0) {...}
val __ficus_version__ = (__ficus_major__, __ficus_minor__, __ficus_patchlevel__)
val __ficus_version_str__ = f"{__ficus_major__}.{__ficus_minor__}.{__ficus_patchlevel__}{__ficus_suffix__}"

exception ASCIIError
exception AssertError
exception BadArgError
exception Break
exception DimError
exception DivByZeroError
exception Exit: int
exception Fail: string
exception FileOpenError
exception IOError
exception NoMatchError
exception NotFoundError
exception NotImplementedError
exception NullFileError
exception NullListError
exception NullPtrError
exception OptionError
exception OutOfMemError
exception OutOfRangeError
exception OverflowError
exception ParallelForError
exception RangeError
exception SizeError
exception SizeMismatchError
exception StackOverflowError
exception TypeMismatchError
exception ZeroStepError

type scalar_t =
    | Notype | Type_Int | Type_I8 | Type_U8
    | Type_I16 | Type_U16 | Type_I32 | Type_U32
    | Type_I64 | Type_U64 | Type_F16 | Type_BF16
    | Type_F32 | Type_F64 | Type_Bool | Type_Char

fun assert(f: bool) = if !f {throw AssertError}
// TODO: probably, we need AssertError to get a string argument or have alternative AssertErrorArg with such a argument
fun assert((f, f_str, fname, lineno): (bool, string, string, int)) = if !f {throw Fail(f"{fname}:{lineno}: Assertion '{f_str}' failed")}

fun ignore(_: 't) {}
@nothrow fun always_use(x: 't): void = @ccode {}

// 't?, int? etc. can be used instead of 't option, int option ...
class 't option = None | Some: 't

type byte = uint8

fun value_or(x: 't?, defval: 't) = match x { | Some(x) => x | _ => defval }
fun value(x: 't?) = match x { | Some(x) => x | _ => throw OptionError }
fun isnone(x: 't?) { | Some _ => false | _ => true }
fun issome(x: 't?) { | Some _ => true | _ => false }

fun __is_scalar__(x: 't) = false
@inline fun __is_scalar__(x: int) = true
@inline fun __is_scalar__(x: int8) = true
@inline fun __is_scalar__(x: uint8) = true
@inline fun __is_scalar__(x: int16) = true
@inline fun __is_scalar__(x: uint16) = true
@inline fun __is_scalar__(x: int32) = true
@inline fun __is_scalar__(x: uint32) = true
@inline fun __is_scalar__(x: int64) = true
@inline fun __is_scalar__(x: uint64) = true
@inline fun __is_scalar__(x: half) = true
@inline fun __is_scalar__(x: float) = true
@inline fun __is_scalar__(x: double) = true
@inline fun __is_scalar__(x: bool) = true
@inline fun __is_scalar__(x: char) = true

@inline fun scalar_type(_: 't) = Notype
@inline fun scalar_type(_: int) = Type_Int
@inline fun scalar_type(_: int8) = Type_I8
@inline fun scalar_type(_: uint8) = Type_U8
@inline fun scalar_type(_: int16) = Type_I16
@inline fun scalar_type(_: uint16) = Type_U16
@inline fun scalar_type(_: int32) = Type_I32
@inline fun scalar_type(_: uint32) = Type_U32
@inline fun scalar_type(_: int64) = Type_I64
@inline fun scalar_type(_: uint64) = Type_U64
@inline fun scalar_type(_: half) = Type_F16
@inline fun scalar_type(_: float) = Type_F32
@inline fun scalar_type(_: double) = Type_F64
@inline fun scalar_type(_: bool) = Type_Bool
@inline fun scalar_type(_: char) = Type_Char

@pure @nothrow fun elemsize(t: scalar_t): int
@ccode { return fx_elemsize(t->tag); }

@inline fun __min__(x: int8) = -128i8
@inline fun __max__(x: int8) = 127i8
@inline fun __min__(x: uint8) = 0u8
@inline fun __max__(x: uint8) = 255u8
@inline fun __min__(x: int16) = -32768i16
@inline fun __max__(x: int16) = 32767i16
@inline fun __min__(x: uint16) = 0u16
@inline fun __max__(x: uint16) = 65535u16
@inline fun __min__(x: int32) = -2147483648i32
@inline fun __max__(x: int32) = 2147483647i32
@inline fun __min__(x: uint32) = 0u32
@inline fun __max__(x: uint32) = 4294967295u32
@inline fun __min__(x: int64) = -9223372036854775807i64 // -922....808 causes a warning
@inline fun __max__(x: int64) = 9223372036854775807i64
@inline fun __min__(x: uint64) = 0u64
@inline fun __max__(x: uint64) = 18446744073709551615u64
@inline fun __min__(x: float) = -3.402823466e+38f
@inline fun __max__(x: float) = 3.402823466e+38f
@inline fun __min__(x: double) = -1.797693134862315e+308
@inline fun __max__(x: double) = 1.797693134862315e+308

operator != (a: 't, b: 't): bool = !(a == b)
operator < (a: 't, b: 't): bool = (a <=> b) < 0
operator > (a: 't, b: 't): bool = (a <=> b) > 0
operator >= (a: 't, b: 't): bool = (a <=> b) >= 0
operator <= (a: 't, b: 't): bool = (a <=> b) <= 0

fun length(s: string) = __intrin_size__(s)
@pure @nothrow fun length(l: 't list): int = @ccode { return fx_list_length(l) }

@pure fun join(sep: string, strs:string []): string
@ccode {
    return fx_strjoin(0, 0, sep, (fx_str_t*)strs->data,
                    strs->dim[0].size, fx_result);
}

@pure fun join_embrace(begin: string, end: string,
        sep: string, strs:string []): string
@ccode {
    return fx_strjoin(begin, end, sep,
        (fx_str_t*)strs->data,
        strs->dim[0].size, fx_result);
}

fun join_embrace(begin: string, end: string,
                 sep: string, strs: string list) =
    join_embrace(begin, end, sep, array(strs))

fun join(sep: string, strs: string list) =
    join(sep, [for s <- strs {s}])

operator + (a: string, b: string): string
@ccode {
    fx_str_t s[] = {*a, *b};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: string, b: char): string
@ccode {
    fx_str_t s[] = {*a, {0, &b, 1}};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: char, b: string): string
@ccode {
    fx_str_t s[] = {{0, &a, 1}, *b};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: char, b: char): string
@ccode {
    char_ cc[] = {a, b};
    return fx_make_str(cc, 2, fx_result);
}

operator + (l1: 't list, l2: 't list)
{
    @nothrow fun link2(l1: 't list, l2: 't list): 't list = @ccode { fx_link_lists(l1, l2, fx_result) }
    match (l1, l2) {
        | ([], _) => l2
        | (_, []) => l1
        | _ => link2([::for x <- l1 {x}], l2)
    }
}

@pure fun string(a: exn): string = @ccode { return fx_exn_to_string(a, fx_result) }
@pure fun string(a: cptr): string
@ccode {
    char buf[64];
    if (a && a->ptr)
        sprintf(buf, "cptr<%p: %p>", a, a->ptr);
    else
        strcpy(buf, "cptr<null>");
    return fx_ascii2str(buf, -1, fx_result);
}
fun string(a: bool) = if a {"true"} else {"false"}
@pure fun string(a: int): string = @ccode  { return fx_itoa(a, false, fx_result) }
@pure fun string(a: long): string = @ccode  { return fx_ltoa(a, 'd', false, fx_result) }
@pure fun string(a: uint8): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int8): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint16): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int16): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint32): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int32): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint64): string = @ccode { return fx_itoa((int64_t)a, true, fx_result) }
@pure fun string(a: int64): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(c: char): string = @ccode { return fx_make_str(&c, 1, fx_result) }
@pure fun string(a: half): string
@ccode {
    char buf[32];
    fx_bits32_t u;
    float f = FX_FLOAT(a);
    u.f = f;
    if ((u.i & 0x7f800000) != 0x7f800000)
        sprintf(buf, (f == (int)f ? "%.1f" : "%.4g"), f);
    else
        strcpy(buf, (u.i & 0x7fffff) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
    return fx_ascii2str(buf, -1, fx_result);
}
@pure fun string(a: float): string
@ccode {
    char buf[32];
    fx_bits32_t u;
    u.f = a;
    if ((u.i & 0x7f800000) != 0x7f800000)
        sprintf(buf, (a == (int)a ? "%.1f" : "%.8g"), a);
    else
        strcpy(buf, (u.i & 0x7fffff) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
    return fx_ascii2str(buf, -1, fx_result);
}
@pure fun string(a: double): string
@ccode {
    char buf[32];
    fx_bits64_t u;
    u.f = a;
    if ((u.i & 0x7FF0000000000000LL) != 0x7FF0000000000000LL)
        sprintf(buf, (a == (int)a ? "%.1f" : "%.16g"), a);
    else
        strcpy(buf, (u.i & 0xfffffffffffffLL) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
    return fx_ascii2str(buf, -1, fx_result);
}
@inline fun string(a: string): string = a

fun string(a: 't?) {
    | Some(a) => "Some(" + repr(a) + ")"
    | _ => "None"
}

fun ord(c: char) = (c :> int)
fun chr(i: int) = (i :> char)

fun odd(i: 't) = i % 2 != 0
fun odd(i: int64) = i % 2 != 0i64
fun odd(i: uint64) = i % 2u64 != 0u64
fun even(i: 't) = i % 2 == 0
fun even(i: int64) = i % 2 == 0i64
fun even(i: uint64) = i % 2u64 == 0u64

fun repr(a: 't): string = string(a)
// [TODO]: move String.escaped() to runtime and call it here
fun repr(a: string) = "\"" + a + "\""
@pure fun repr(a: char): string = @ccode {
    char_ buf[16] = {39, 92, 39};
    if (32 <= a && a != 39 && a != 34 && a != 92) {
        buf[1] = a;
        return fx_make_str(buf, 3, fx_result);
    } else {
        int k, len = 4;
        buf[3] = 39;
        switch(a) {
        case 10: buf[2]='n'; break;
        case 13: buf[2]='r'; break;
        case 9: buf[2]='t'; break;
        case 0: buf[2]='0'; break;
        case 39: buf[2]=39; break;
        case 34: buf[2]=34; break;
        case 92: buf[2]=92; break;
        default:
            if (a < 128) {
                len = 2;
                buf[2] = 'x';
            } else if (a < 65536) {
                len = 4;
                buf[2] = 'u';
            } else {
                len = 8;
                buf[2] = 'U';
            }
            for(k = 0; k < len; k++) {
                unsigned denom = 1U << ((len-k-1)*4);
                unsigned d = (unsigned)a/denom;
                a = (char_)(a%denom);
                buf[k+3] = (char_)(d > 10 ? (d - 10 + 'A') : (d + '0'));
            }
            len += 4;
            buf[len-1] = '\'';
            printf("len=%d: \n", len);
            for (int i = 0; i < len; i++)
                printf("%c", buf[i]);
            printf("\n");
        }
        return fx_make_str(buf, len, fx_result);
    }
}

fun string(t: (...)) = join_embrace("(", ")", ", ", [for x <- t {repr(x)}])
fun string(r: {...}) = join_embrace("{", "}", ", ", [for (n, x) <- r {n+"="+repr(x)}])
fun string(a: 't ref) = "ref(" + repr(*a) + ")"

fun string(a: 't [])
{
    join_embrace("[", "]", ", ", [for x <- a {repr(x)}])
}

fun string(a: 't [,])
{
    val (m, n) = size(a)
    val rows = [for i <- 0:m {
        val elems = [for j <- 0:n {repr(a[i, j])}]
        join(", ", elems)
    }]
    join_embrace("[", "]", ";\n", rows)
}

fun string(a: 't [,,])
{
    val (d, m, n) = size(a)
    val planes = [for k <- 0:d {
        val rows = [for i <- 0:m {
            val elems = [for j <- 0:n {repr(a[k, i, j])}]
            join(", ", elems)
        }]
        join_embrace("", "", ";\n", rows)
    }]
    join_embrace("[", "]", ";;\n\n", planes)
}

fun string(l: 't list) = join_embrace("[", "]", ", ", [for x <- l {repr(x)}])

@pure fun string(a: char []): string = @ccode {
    return fx_make_str((char_*)a->data, a->dim[0].size, fx_result);
}

fun string(v: 't vector) = join_embrace("[", "]", ", ", [for x <- v {repr(x)}])

@pure fun string(v: char vector): string
@ccode {
    int_ i, size = v->size;
    int fx_status = fx_make_str(0, size, fx_result);
    if (fx_status >= 0) {
        fx_rrbiter_t iter;
        char_* src = FX_RRB_START_READ(char_, *v, iter);
        char_* dst = fx_result->data;
        for (i = 0; i < size; i++) {
            dst[i] = *src;
            FX_RRB_NEXT(char_, iter, src);
        }
    }
    return fx_status;
}

fun string(typ: scalar_t)
{
    | Notype => "Notype"
    | Type_Int => "Int"
    | Type_I8 => "I8"
    | Type_U8 => "U8"
    | Type_I16 => "I16"
    | Type_U16 => "U16"
    | Type_I32 => "I32"
    | Type_U32 => "U32"
    | Type_I64 => "I64"
    | Type_U64 => "U64"
    | Type_F16 => "F16"
    | Type_BF16 => "BF16"
    | Type_F32 => "F32"
    | Type_F64 => "F64"
    | Type_Bool => "Bool"
    | Type_Char => "Char"
}

fun repr(v: char vector) =
    join_embrace("[", "]", ", ", [for x <- v {repr(x)}])

type format_t =
{
    fill: char=' '
    align: char=' '
    sign: char='-'
    num_alt: bool=false
    width: int=0
    precision: int=-1
    grouping: char=' '
    typ: char=' '
}

@pure fun parse_format(fmt: string, ~start: int=0): (format_t, int)
@ccode {
    FX_STATIC_ASSERT(sizeof(fx_result->t0) == sizeof(fx_format_t));
    return fx_parse_format(fmt, start, (fx_format_t*)&fx_result->t0, &fx_result->t1);
}
fun string(x: 't, fmt: format_t) = repr(x)
fun string(x: 't, fmt: string) = string(x, parse_format(fmt).0)
@pure fun format_(x: int64, u: bool, fmt: format_t): string
@ccode {
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_int(x, u, (fx_format_t*)fmt, fx_result);
}
@pure fun format_(x: double, precision0: int, fmt: format_t): string
@ccode {
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_flt(x, precision0, (fx_format_t*)fmt, fx_result);
}
fun string(x: int, fmt: format_t) = format_(x :> int64, false, fmt)
fun string(x: int8, fmt: format_t) = format_(x :> int64, false, fmt)
fun string(x: uint8, fmt: format_t) = format_(x :> int64, true, fmt)
fun string(x: int16, fmt: format_t) = format_(x :> int64, false, fmt)
fun string(x: uint16, fmt: format_t) = format_(x :> int64, true, fmt)
fun string(x: int32, fmt: format_t) = format_(x :> int64, false, fmt)
fun string(x: uint32, fmt: format_t) = format_(x :> int64, true, fmt)
fun string(x: int64, fmt: format_t) = format_(x, false, fmt)
fun string(x: uint64, fmt: format_t) = format_(x :> int64, true, fmt)
fun string(x: float, fmt: format_t) = format_(x :> double, 8, fmt)
fun string(x: double, fmt: format_t) = format_(x, 16, fmt)
fun string(x: half, fmt: format_t) = format_(x :> double, 4, fmt)
@pure fun string(x: long, fmt: format_t): string
@ccode {
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_long(x, (fx_format_t*)fmt, fx_result);
}
@pure fun string(x: string, fmt: format_t): string
@ccode {
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_str(x, (fx_format_t*)fmt, fx_result);
}
@pure fun string(x: bool, fmt: format_t): string
@ccode {
    fx_str_t false_str = FX_MAKE_STR("false");
    fx_str_t true_str = FX_MAKE_STR("true");
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_str(x ? &true_str : &false_str, (fx_format_t*)fmt, fx_result);
}
@pure fun string(x: char, fmt: format_t): string
@ccode {
    fx_str_t x_str = FX_MAKE_VAR_STR1(x);
    FX_STATIC_ASSERT(sizeof(*fmt) == sizeof(fx_format_t));
    return fx_format_str(&x_str, (fx_format_t*)fmt, fx_result);
}

fun string(t: (...), fmt: format_t) = join_embrace("(", ")", ", ", [for x <- t {string(x, fmt)}])
fun string(r: {...}, fmt: format_t) = join_embrace("{", "}", ", ", [for (n, x) <- r {n+"="+string(x, fmt)}])
fun string(a: 't ref, fmt: format_t) = "ref(" + format(a, fmt) + ")"

fun string(a: 't [], fmt: format_t)
{
    join_embrace("[", "]", ", ", [for x <- a {string(x, fmt)}])
}

fun string(a: 't [,], fmt: format_t)
{
    val (m, n) = size(a)
    val rows = [for i <- 0:m {
        val elems = [for j <- 0:n {string(a[i, j], fmt)}]
        join(", ", elems)
    }]
    join_embrace("[", "]", ";\n", rows)
}

fun string(a: 't [,,], fmt: format_t)
{
    val (d, m, n) = size(a)
    val planes = [for k <- 0:d {
        val rows = [for i <- 0:m {
            val elems = [for j <- 0:n {string(a[k, i, j], fmt)}]
            join(", ", elems)
        }]
        join_embrace("", "", ";\n", rows)
    }]
    join_embrace("[", "]", ";;\n\n", planes)
}

fun string(l: 't list, fmt: format_t) = join_embrace("[", "]", ", ", [for x <- l {string(x, fmt)}])
fun string(a: char [], fmt: format_t) = string(string(a), fmt)
fun string(v: 't vector, fmt: format_t) = join_embrace("[", "]", ", ", [for x <- v {string(x, fmt)}])
fun string(v: char vector, fmt: format_t) = string(string(v), fmt)

@pure operator * (c: char, n: int): string
@ccode {
    int fx_status = fx_make_str(0, n, fx_result);
    if (fx_status >= 0) {
        for( int_ i = 0; i < n; i++ )
            fx_result->data[i] = c;
    }
    return fx_status;
}

@pure operator * (s: string, n: int): string
@ccode {
    int_ sz = s->length;
    int fx_status = fx_make_str(0, n*sz, fx_result);
    if (fx_status >= 0 && (n*sz) > 0) {
        for( int_ i = 0; i < n; i++ )
            memcpy(fx_result->data + i*sz, s->data, sz*sizeof(s->data[0]));
    }
    return fx_status;
}

operator * (n: int, c: char) = c * n
operator * (n: int, s: string) = s * n

operator == (a: 't list, b: 't list): bool
{
    a === b ||
    (match (a, b) {
    | (ai :: a, bi :: b) => ai == bi && a == b
    | ([], []) => true
    | _ => false
    })
}

operator <=> (a: 't list, b: 't list): int
{
    | (ai :: a, bi :: b) =>
        val d = ai <=> bi
        if d != 0 {d} else {a <=> b}
    | ([], _ :: _) => -1
    | (_ :: _, []) => 1
    | _ => 0
}

operator == (a: 't vector, b: 't vector): bool
{
    size(a) == size(b) &&
    all(for xa <- a, xb <- b {xa == xb})
}

operator <=> (a: 't vector, b: 't vector): int
{
    val na = size(a), nb = size(b)
    val n = min(na, nb)
    var d = 0
    for i <- 0:n {
        d = a[i] <=> b[i]
        if d != 0 {break}
    }
    if d != 0 {d} else {na <=> nb}
}

operator == (a: 't [+], b: 't [+]): bool =
    size(a) == size(b) &&
    all(for xa <- a, xb <- b {xa == xb})

operator <=> (a: 't [], b: 't []): int
{
    val na = size(a), nb = size(b)
    val n = min(na, nb)
    var d = 0
    for i <- 0:n {
        d = a[i] <=> b[i]
        if d != 0 {break}
    }
    if d != 0 {d} else {na <=> nb}
}

// [TODO] add __intrin_tuple_size(x) that is expanded to a constant at compile time.
fun size(_: ('t*2)) = 2
fun size(_: ('t*3)) = 3
fun size(_: ('t*4)) = 4
fun size(_: ('t*5)) = 5

operator == (a: (...), b: (...)) =
    fold f=true for aj <- a, bj <- b {if aj == bj {f} else {false}}
operator <=> (a: (...), b: (...)) =
    fold d=0 for aj <- a, bj <- b {if d != 0 {d} else {aj <=> bj}}
operator == (a: {...}, b: {...}) =
    fold f=true for (_, aj) <- a, (_, bj) <- b {if aj == bj {f} else {false}}
operator <=> (a: {...}, b: {...}) =
    fold d=0 for (_, aj) <- a, (_, bj) <- b {if d != 0 {d} else {aj <=> bj}}

operator <=> (a: int, b: int): int = (a > b) - (a < b)
operator <=> (a: int8, b: int8): int = (a > b) - (a < b)
operator <=> (a: uint8, b: uint8): int = (a > b) - (a < b)
operator <=> (a: int16, b: int16): int = (a > b) - (a < b)
operator <=> (a: uint16, b: uint16): int = (a > b) - (a < b)
operator <=> (a: int32, b: int32): int = (a > b) - (a < b)
operator <=> (a: uint32, b: uint32): int = (a > b) - (a < b)
operator <=> (a: int64, b: int64): int = (a > b) - (a < b)
operator <=> (a: uint64, b: uint64): int = (a > b) - (a < b)
operator <=> (a: float, b: float): int = (a > b) - (a < b)
operator <=> (a: double, b: double): int = (a > b) - (a < b)
operator <=> (a: char, b: char): int = (a > b) - (a < b)
operator <=> (a: bool, b: bool): int = (a > b) - (a < b)
@pure @nothrow operator <=> (a: long, b: long): int
@ccode { return fx_long_cmp(a, b) }
@pure @nothrow operator == (a: long, b: long): int
@ccode { return fx_long_cmp(a, b) == 0 }

operator + (a: long, b: long): long
@ccode { return fx_long_add(a, b, fx_result) }
operator - (a: long, b: long): long
@ccode { return fx_long_sub(a, b, fx_result) }
operator * (a: long, b: long): long
@ccode { return fx_long_mul(a, b, fx_result) }
operator / (a: long, b: long): long
@ccode { return fx_long_div(a, b, fx_result) }
operator % (a: long, b: long): long
@ccode { return fx_long_mod(a, b, fx_result) }
operator & (a: long, b: long): long
@ccode { return fx_long_and(a, b, fx_result) }
operator | (a: long, b: long): long
@ccode { return fx_long_or(a, b, fx_result) }
operator ^ (a: long, b: long): long
@ccode { return fx_long_xor(a, b, fx_result) }
fun __negate__ (a: long): long
@ccode { return fx_long_neg(a, fx_result) }
fun abs(a: long): long
@ccode { return fx_long_abs(a, fx_result) }
@pure @nothrow fun sign(a: long): int
@ccode { return fx_long_sign(a, fx_result) }

operator .* (a: ('t...), b: 'ts) = (for aj <- a {aj * b})
operator ./ (a: ('t...), b: 'ts) = (for aj <- a {aj / b})
operator .* (a: 'ts, b: ('t...)) = (for bj <- b {a * bj})
operator ./ (a: 'ts, b: ('t...)) = (for bj <- b {a / bj})
operator + (a: (...), b: (...)) = (for aj <- a, bj <- b {aj + bj})
operator - (a: (...), b: (...)) = (for aj <- a, bj <- b {aj - bj})
operator .+ (a: (...), b: (...)) = (for aj <- a, bj <- b {aj + bj})
operator .- (a: (...), b: (...)) = (for aj <- a, bj <- b {aj - bj})
operator .* (a: (...), b: (...)) = (for aj <- a, bj <- b {aj * bj})
operator ./ (a: (...), b: (...)) = (for aj <- a, bj <- b {aj / bj})
operator | (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj | bj})
operator & (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj & bj})
operator ^ (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj ^ bj})

// complex multiplication
operator * (a: ('t*2), b: ('t*2)) =
    (a.0*b.0 - a.1*b.1, a.0*b.1 + a.1*b.0)
// and division
operator / (a: ('t*2), b: ('t*2))
{
    val q = b.0*b.0 + b.1*b.1
    ((a.0*b.0 + a.1*b.1)/q, (a.1*b.0 - a.0*b.1)/q)
}

// quaternion product
operator * (a: ('t*4), b: ('t*4)) =
    (a.0*b.0 - a.1*b.1 - a.2*b.2 - a.3*b.3,
    a.0*b.1 + a.1*b.0 + a.2*b.3 - a.3*b.2,
    a.0*b.2 - a.1*b.3 + a.2*b.0 + a.3*b.1,
    a.0*b.3 + a.1*b.2 - a.2*b.1 + a.3*b.0)

// dot product
fun dot(a: ('t...), b: ('t...)) =
    fold s = a.0-a.0 for aj <- a, bj <- b {s + aj*bj}

// cross product
fun cross(a: ('t*2), b: ('t*2)) = a.0*b.1 - a.1*b.0
fun cross(a: ('t*3), b: ('t*3)) =
    (a.1*b.2 - a.2*b.1, a.2*b.0 - a.0*b.2, a.0*b.1 - a.1*b.0)

fun normInf(a: 't, b: 't) = abs(a - b)
fun normInf(a: 't) = abs(a)
fun normL1(a: 't, b: 't) = abs(a - b)
fun normL1(a: 't) = abs(a)
fun normL2sqr(a: 't, b: 't) { val t = abs(a - b); t*t }
fun normL2sqr(a: 't) { val t = abs(a); t*t }
fun normL2(a: 't) = abs(a)

fun normInf(a: ('t...)) =
    fold s = normInf(a.0) for aj <- a {max(s, normInf(aj))}
fun normL1(a: ('t...)) =
    fold s = normL1(a.0)*0.f for aj <- a {s + normL1(aj)}
fun normL2sqr(a: ('t...)) =
    fold s = normL1(a.0)*0.f for aj <- a {s + normL2sqr(aj)}
fun normL2(a: ('t...)) = __intrin_sqrt__(normL2sqr(a))
fun normInf(a: ('t...), b: ('t...)) =
    fold s = normInf(a.0, b.0) for aj <- a, bj <- b {max(s, normInf(aj, bj))}
fun normL1(a: ('t...), b: ('t...)) =
    fold s = normL1(a.0)*0.f for aj <- a, bj <- b {s + normL1(aj, bj)}
fun normL2sqr(a: ('t...), b: ('t...)) =
    fold s = normL1(a.0)*0.f for aj <- a, bj <- b {s + normL2sqr(aj, bj)}
fun normL2(a: ('t...), b: ('t...)) = __intrin_sqrt__(normL2sqr(a, b))

fun norm(a: ('t...)) = normL2(a)
fun norm(a: ('t...), b: ('t...)) = normL2(a, b)

operator .== (a: ('t...), b: 't): (bool...) = (for aj <- a {aj == b})
operator .!= (a: ('t...), b: 't): (bool...) = (for aj <- a {aj != b})
operator .< (a: ('t...), b: 't): (bool...) = (for aj <- a {aj < b})
operator .<= (a: ('t...), b: 't): (bool...) = (for aj <- a {aj <= b})
operator .> (a: ('t...), b: 't): (bool ...) = (for aj <- a {aj > b})
operator .>= (a: ('t...), b: 't): (bool...) = (for aj <- a {aj >= b})

operator .== (b: 't, a: ('t...)): (bool...) = a .== b
operator .!= (b: 't, a: ('t...)): (bool...) = a .!= b
operator .< (b: 't, a: ('t...)): (bool...) = a .> b
operator .<= (b: 't, a: ('t...)): (bool...) = a .>= b
operator .> (b: 't, a: ('t...)): (bool...) = a .< b
operator .>= (b: 't, a: ('t...)): (bool...) = a .<= b

operator .== (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj == bj})
operator .!= (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj != bj})
operator .< (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj < bj})
operator .<= (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj <= bj})
operator .> (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj > bj})
operator .>= (a: ('t...), b: ('t...)): (bool...) = (for aj <- a, bj <- b {aj >= bj})

operator == (a: 't?, b: 't?) {
    | (Some(a), Some(b)) => a == b
    | (None, None) => true
    | _ => false
}

operator <=> (a: 't?, b: 't?) {
    | (Some(a), Some(b)) => a <=> b
    | (None, Some _) => -1
    | (Some _, None) => 1
    | _ => 0
}

fun all(a: (bool...)) = fold f=true for x <- a {f & x}
fun exists(a: (bool...)) = fold f=false for x <- a {f | x}

@pure @nothrow operator == (a: string, b: string): bool = @ccode { return fx_streq(a, b); }

// [TODO] implement more clever string comparison operation
@pure @nothrow operator <=> (a: string, b: string): int
@ccode {
    int_ alen = a->length, blen = b->length;
    int_ minlen = alen < blen ? alen : blen;
    const char_ *adata = a->data, *bdata = b->data;
    for(int_ i = 0; i < minlen; i++) {
        int_ ai = (int_)adata[i], bi = (int_)bdata[i], diff = ai - bi;
        if(diff != 0)
            return diff > 0 ? 1 : -1;
    }
    return alen < blen ? -1 : alen > blen;
}

// compare the pointers, not the content. Maybe need a separate operator for that.
@pure @nothrow operator == (a: 't ref, b: 't ref): bool = @ccode { return a == b }
@pure @nothrow operator == (a: cptr, b: cptr): bool = @ccode { return a == b }

// these are pseudo-functions that are treated specially by the compiler
@pure @nothrow fun __eq_variants__(a: 't, b: 't): bool = a.__tag__ == b.__tag__
fun __fun_string__(a: 't): string
@ccode {
    char buf[32];
    sprintf(buf, "func<%p: %p>", a->fp, a->fcv);
    return fx_ascii2str(buf, -1, fx_result);
}

// operator === checks whether a and b represent not just equal, but the same object.
// For many of the types it's should be enough to check "a == b" at C level.
// But for some other types it may be not enough.
@pure @nothrow operator === (a: 't, b: 't): bool = @ccode {return a == b}
@pure @nothrow operator === (a: string, b: string): bool = @ccode {return a->data == b->data}
@pure @nothrow operator === (a: 't [+], b: 't [+]): bool = @ccode {return a->data == b->data}
operator === (a: (...), b: (...)): bool =
    fold f=true for aj<-a, bj<-b {f & (aj === bj)}
operator === (a: {...}, b: {...}): bool =
    fold f=true for (_, aj)<-a, (_, bj)<-b {f & (aj === bj)}
operator === (a: 't?, b: 't?) {
    | (Some(a), Some(b)) => a === b
    | (None, None) => true
    | _ => false
}

fun int(x: 't) = (x :> int)
fun uint8(x: 't) = (x :> uint8)
fun int8(x: 't) = (x :> int8)
fun uint16(x: 't) = (x :> uint16)
fun int16(x: 't) = (x :> int16)
fun uint32(x: 't) = (x :> uint32)
fun int32(x: 't) = (x :> int32)
fun uint64(x: 't) = (x :> uint64)
fun int64(x: 't) = (x :> int64)
fun float(x: 't) = (x :> float)
fun double(x: 't) = (x :> double)
fun half(x: 't) = (x :> half)

@pure fun int(x: long): int
@ccode { return fx_ltoi(x, fx_result) }

@pure @nothrow fun long(x: int64): long
@ccode { FX_MAKE_LONG(x, fx_result); }
@pure @nothrow fun long(x: uint64): long
@ccode { FX_MAKE_ULONG(x, fx_result); }
fun long(x: int8) = long(x :> int64)
fun long(x: uint8) = long(x :> int64)
fun long(x: int16) = long(x :> int64)
fun long(x: uint16) = long(x :> int64)
fun long(x: int32) = long(x :> int64)
fun long(x: uint32) = long(x :> int64)
fun long(x: int) = long(x :> int64)
fun long(x: bool) = long(x :> int64)
fun long(x: string): long
@ccode { return fx_atol(x, 0, fx_result) }

fun int(x: ('t...)) = (for xj <- x {int(xj)})
fun uint8(x: ('t...)) = (for xj <- x {uint8(xj)})
fun int8(x: ('t...)) = (for xj <- x {int8(xj)})
fun uint16(x: ('t...)) = (for xj <- x {uint16(xj)})
fun int16(x: ('t...)) = (for xj <- x {int16(xj)})
fun uint32(x: ('t...)) = (for xj <- x {uint32(xj)})
fun int32(x: ('t...)) = (for xj <- x {int32(xj)})
fun uint64(x: ('t...)) = (for xj <- x {uint64(xj)})
fun int64(x: ('t...)) = (for xj <- x {int64(xj)})
fun float(x: ('t...)) = (for xj <- x {float(xj)})
fun double(x: ('t...)) = (for xj <- x {double(xj)})
fun half(x: ('t...)) = (for xj <- x {half(xj)})

fun int(x: 't [+]) = [for xj <- x {int(xj)}]
fun uint8(x: 't [+]) = [for xj <- x {uint8(xj)}]
fun int8(x: 't [+]) = [for xj <- x {int8(xj)}]
fun uint16(x: 't [+]) = [for xj <- x {uint16(xj)}]
fun int16(x: 't [+]) = [for xj <- x {int16(xj)}]
fun uint32(x: 't [+]) = [for xj <- x {uint32(xj)}]
fun int32(x: 't [+]) = [for xj <- x {int32(xj)}]
fun uint64(x: 't [+]) = [for xj <- x {uint64(xj)}]
fun int64(x: 't [+]) = [for xj <- x {int64(xj)}]
fun float(x: 't [+]) = [for xj <- x {float(xj)}]
fun double(x: 't [+]) = [for xj <- x {double(xj)}]
fun half(x: 't [+]) = [for xj <- x {half(xj)}]

type uint8x3 = (uint8*3)
type uint8x4 = (uint8*4)
type uint16x3 = (uint16*3)
type uint16x4 = (uint16*4)
type intx2 = (int*2)
type intx3 = (int*3)
type intx4 = (int*4)
type intx5 = (int*5)
type intx6 = (int*6)
type int32x2 = (int32*2)
type int32x3 = (int32*3)
type int32x4 = (int32*4)
type floatx2 = (float*2)
type floatx3 = (float*3)
type floatx4 = (float*4)
type floatx5 = (float*5)
type floatx6 = (float*6)
type doublex2 = (double*2)
type doublex3 = (double*3)
type doublex4 = (double*4)
type doublex5 = (double*5)
type doublex6 = (double*6)

/*
@inline fun sat_uint8(i: int) = __intrin_sat_uint8__(i)
@inline fun sat_uint8(f: float) = __intrin_sat_uint8__(f)
@inline fun sat_uint8(d: double) = __intrin_sat_uint8__(d)

@inline fun sat_int8(i: int) = __intrin_sat_int8__(i)
@inline fun sat_int8(f: float) = __intrin_sat_int8__(f)
@inline fun sat_int8(d: double) = __intrin_sat_int8__(d)

@inline fun sat_uint16(i: int) = __intrin_sat_uint16__(i)
@inline fun sat_uint16(f: float) = __intrin_sat_uint16__(f)
@inline fun sat_uint16(d: double) = __intrin_sat_uint16__(d)

@inline fun sat_int16(i: int) = __intrin_sat_int16__(i)
@inline fun sat_int16(f: float) = __intrin_sat_int16__(f)
@inline fun sat_int16(d: double) = __intrin_sat_int16__(d)
*/

@pure @nothrow fun sat_uint8(i: int): uint8
@ccode { return (unsigned char)((i & ~255) == 0 ? i : i < 0 ? 0 : 255); }

@pure @nothrow fun sat_uint8(f: float): uint8
@ccode {
    int i = fx_roundf2i(f);
    return (unsigned char)((i & ~255) == 0 ? i : i < 0 ? 0 : 255);
}

@pure @nothrow fun sat_uint8(d: double): uint8
@ccode {
    int_ i = fx_round2I(d);
    return (unsigned char)((i & ~255) == 0 ? i : i < 0 ? 0 : 255);
}

@pure @nothrow fun sat_int8(i: int): int8
@ccode { return (signed char)(((i + 128) & ~255) == 0 ? i : i < 0 ? -128 : 127); }

@pure @nothrow fun sat_int8(f: float): int8
@ccode {
    int i = fx_roundf2i(f);
    return (signed char)(((i + 128) & ~255) == 0 ? i : i < 0 ? -128 : 127);
}

@pure @nothrow fun sat_int8(d: double): int8
@ccode {
    int_ i = fx_round2I(d);
    return (signed char)(((i + 128) & ~255) == 0 ? i : i < 0 ? -128 : 127);
}

@pure @nothrow fun sat_uint16(i: int): uint16
@ccode {
    return (unsigned short)((i & ~65535) == 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_uint16(f: float): uint16
@ccode {
    int i = fx_roundf2i(f);
    return (unsigned short)((i & ~65535) == 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_uint16(d: double): uint16
@ccode {
    int_ i = fx_round2I(d);
    return (unsigned short)((i & ~65535) == 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_int16(i: int): int16
@ccode {
    return (short)(((i+32768) & ~65535) == 0 ? i : i < 0 ? -32768 : 32767);
}

@pure @nothrow fun sat_int16(f: float): int16
@ccode {
    int i = fx_roundf2i(f);
    return (short)(((i+32768) & ~65535) == 0 ? i : i < 0 ? -32768 : 32767);
}

@pure @nothrow fun sat_int16(d: double): int16
@ccode {
    int_ i = fx_round2I(d);
    return (short)(((i+32768) & ~65535) == 0 ? i : i < 0 ? -32768 : 32767);
}

fun sat_uint8(x: ('t...)) = (for xj <- x {sat_uint8(xj)})
fun sat_int8(x: ('t...)) = (for xj <- x {sat_int8(xj)})
fun sat_uint16(x: ('t...)) = (for xj <- x {sat_uint16(xj)})
fun sat_int16(x: ('t...)) = (for xj <- x {sat_int16(xj)})

// do not use lrint(x), since it's slow. and (int)round(x) is even slower
@pure @nothrow fun round(x: float): int = @ccode { return fx_roundf2I(x) }
@pure @nothrow fun round(x: double): int = @ccode { return fx_round2I(x) }
@pure @nothrow fun round(x: float, n: int): float
@ccode {
    double scale =
        n ==  0 ? 1   : n == 1 ? 1e1 : n == 2 ? 1e2 : n == 3 ? 1e3 :
        n ==  4 ? 1e4 : n == 5 ? 1e5 : n == 6 ? 1e6 : n == 7 ? 1e7 : 1e8;
    return (float)(fx_round2I(x*scale)/scale);
}
@pure @nothrow fun round(x: double, n: int): double
@ccode {
    double scale =
        n ==  0 ? 1    : n ==  1 ? 1e1  : n ==  2 ? 1e2  : n ==  3 ? 1e3  :
        n ==  4 ? 1e4  : n ==  5 ? 1e5  : n ==  6 ? 1e6  : n ==  7 ? 1e7  :
        n ==  8 ? 1e8  : n ==  9 ? 1e9  : n == 10 ? 1e10 : n == 11 ? 1e11 :
        n == 12 ? 1e12 : n == 13 ? 1e13 : n == 14 ? 1e14 : n == 15 ? 1e15 : 1e16;
    return fx_round2I(x*scale)/scale;
}

fun round(x: ('t...)) = (for xj <- x {round(xj)})

fun min(a: 't, b: 't) = if a <= b {a} else {b}
fun max(a: 't, b: 't) = if a >= b {a} else {b}
@inline fun min(a: int, b: int) = __intrin_min__(a, b)
@inline fun max(a: int, b: int) = __intrin_max__(a, b)
@inline fun min(a: float, b: float) = __intrin_min__(a, b)
@inline fun max(a: float, b: float) = __intrin_max__(a, b)
@inline fun min(a: double, b: double) = __intrin_min__(a, b)
@inline fun max(a: double, b: double) = __intrin_max__(a, b)
fun abs(a: 't) = if a >= (0 :> 't) {a} else {-a}
@inline fun abs(a: uint8) = int(a)
@inline fun abs(a: int8) = if a >= 0 {int(a)} else {-a}
@inline fun abs(a: uint16) = int(a)
@inline fun abs(a: int16) = if a >= 0 {int(a)} else {-a}
@inline fun abs(a: uint32) = a
@inline fun abs(a: int32) = if a >= 0 {int(a)} else {int(-a)}
@inline fun abs(a: uint64) = a
fun sign(a: 't) = int(a > (0 :> 't)) - int(a < (0 :> 't))
fun clip(x: 't, a: 't, b: 't) = if a <= x <= b {x} else if x < a {a} else {b}

@nothrow fun print_string(a: string): void = @ccode { fx_fputs(stdout, a) }

fun print(a: 't) = print_string(string(a))
@nothrow fun print(a: bool): void = @ccode { fputs(a ? "true" : "false", stdout) }
@nothrow fun print(a: int): void = @ccode { printf("%zd", a) }
@nothrow fun print(a: uint8): void = @ccode { printf("%d", (int)a) }
@nothrow fun print(a: int8): void = @ccode { printf("%d", (int)a) }
@nothrow fun print(a: uint16): void = @ccode { printf("%d", (int)a) }
@nothrow fun print(a: int16): void = @ccode { printf("%d", (int)a) }
@nothrow fun print(a: uint32): void = @ccode { printf("%u", a) }
@nothrow fun print(a: int32): void = @ccode { printf("%d", a) }
@nothrow fun print(a: uint64): void = @ccode { printf("%llu", a) }
@nothrow fun print(a: int64): void = @ccode { printf("%lld", a) }
fun print(a: long): void = @ccode {
    fx_cstr_t str;
    int fx_status = fx_ltoa_ascii(a, 'd', false, &str);
    if (fx_status >= 0) {
        printf("%s", str.data);
        fx_free_cstr(&str);
    }
    return fx_status;
}
@nothrow fun print(a: half): void
@ccode {
    fx_bits32_t u;
    float f = FX_FLOAT(a);
    u.f = f;
    if ((u.i & 0x7f800000) != 0x7f800000)
        printf((f == (int)f ? "%.1f" : "%.4g"), f);
    else
        printf((u.i & 0x7fffff) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
}
@nothrow fun print(a: float): void
@ccode {
    fx_bits32_t u;
    u.f = a;
    if ((u.i & 0x7f800000) != 0x7f800000)
        printf((a == (int)a ? "%.1f" : "%.8g"), a);
    else
        printf((u.i & 0x7fffff) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
}
@nothrow fun print(a: double): void
@ccode {
    fx_bits64_t u;
    u.f = a;
    if ((u.i & 0x7ff0000000000000LL) != 0x7ff0000000000000LL)
        printf((a == (int)a ? "%.1f" : "%.16g"), a);
    else
        printf((u.i & 0xfffffffffffffLL) != 0 ? "nan" : u.i > 0 ? "inf" : "-inf");
}
@nothrow fun print(a: cptr): void
@ccode {
    if (a && a->ptr)
        printf("cptr<%p: %p>", a, a->ptr);
    else
        printf("cptr<null>");
}
@inline fun print(a: string) = print_string(a)
fun print_repr(a: 't) = print(a)
fun print_repr(a: string) { print("\""); print(a); print("\"") }
fun print_repr(a: char) { print("'"); print(a); print("'") }
fun print(a: 't [])
{
    print("[")
    for x@i <- a {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}

@nothrow fun print(a: char []): void = @ccode {
    fx_str_t str = {0, (char_*)a->data, a->dim[0].size};
    fx_fputs(stdout, &str)
}

fun print(a: 't [,])
{
    print("[")
    val (m, n) = size(a)
    for i <- 0:m {
        for j <- 0:n {
            if j > 0 {print(", ")}
            print_repr(a[i,j])
        }
        if i < m-1 {print(";\n ")}
    }
    print("]")
}

fun print(l: 't list)
{
    print("[")
    for x@i <- l {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}

fun print(v: 't vector)
{
    print("[")
    for x@i <- v {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}

fun print(t: (...))
{
    print("(");
    for x@i <- t {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print(")")
}

fun print(r: {...})
{
    print("{")
    for (n, x)@i <- r {
        if i > 0 {print(", ")}
        print(n + "=");
        print_repr(x)
    }
    print("}")
}

fun print(a: 't ref) {
    print("ref("); print_repr(*a); print(")")
}

@inline fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }

fun list(a: 't []): 't list = [:: for x <- a {x}]
fun list(s: string): char list = [:: for x <- s {x}]
fun list(v: 't vector): 't list = [:: for x <- v {x}]

fun array(): 't [] = []
fun array(n: int, x: 't) = [for i <- 0:n {x}]
fun array((m: int, n: int), x: 't) = [for i <- 0:m for j <- 0:n {x}]
fun array((m: int, n: int, n2: int), x: 't) = [for i <- 0:m for j <- 0:n for k <- 0:l {x}]
fun array((m: int, n: int, n2: int, n3: int), x: 't) = [for i <- 0:m for j <- 0:n for k <- 0:n2 for l <- 0:n3 {x}]
fun array(l: 't list): 't [] = [for x <- l {x}]
fun array(v: 't vector): 't [] = [for x <- v {x}]
fun array(s: string): char [] = [for x <- s {x}]

// basically, this is violation of the type system; use with care
@nothrow fun reinterpret(x: 'from [+]): 'to [+]
@ccode {
    fx_copy_arr(x, fx_result);
}

fun vector(): 't vector = []
fun vector(l: 't list): 't vector = vector(for x <- l {x})
fun vector(a: 't [+]): 't vector = vector(for x <- a {x})
fun vector(s: string): char vector = vector(for x <- s {x})

fun size(a: 't vector) = __intrin_size__(a)

@ccode {

typedef uint64_t hash_t;
#define FNV_1A_PRIME 1099511628211ULL
#define FNV_1A_OFFSET 14695981039346656037ULL

}

type hash_t = uint64
val FNV_1A_PRIME: hash_t = 1099511628211u64
val FNV_1A_OFFSET: hash_t = 14695981039346656037u64

fun hash(x: (...)): hash_t =
    fold h = FNV_1A_OFFSET for xj <- x {
        val h = h ^ hash(xj)
        h * FNV_1A_PRIME
    }

fun hash(x: {...}): hash_t =
    fold h = FNV_1A_OFFSET for (_, xj) <- x {
        val h = h ^ hash(xj)
        h * FNV_1A_PRIME
    }

@inline fun hash(x: int) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: int32) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: uint32) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: int64) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: uint64) = x ^ FNV_1A_OFFSET
@inline fun hash(x: int8) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: uint8) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: int16) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: uint16) = uint64(x) ^ FNV_1A_OFFSET
@inline fun hash(x: bool) = uint64(x) ^ FNV_1A_OFFSET
@pure @nothrow fun hash(x: float): hash_t = @ccode {
    fx_bits32_t u; u.f = x; return u.u ^ 14695981039346656037ULL;
}
@pure @nothrow fun hash(x: double): hash_t = @ccode {
    fx_bits64_t u; u.f = x;
    return u.u ^ 14695981039346656037ULL;
}
@pure @nothrow fun hash(x: string): hash_t = @ccode {
    uint64_t hash = FNV_1A_OFFSET;
    int_ i, len = x->length;
    char_* data = x->data;
    for(i = 0; i < len; i++) {
        hash ^= data[i];
        hash *= FNV_1A_PRIME;
    }
    return hash;
}

// [TODO] make it an intrinsic
// helper function used in sorting, shuffling and possibly some other array processing algorithms.
// it does _not_ perform range checking for better efficiency, so, please, use with care.
@nothrow fun _swap_(arr: 't [], i: int, j: int): void
@ccode {
    size_t esz = arr->dim[0].step;
    if(esz % sizeof(int) == 0) {
        int* ptr0 = (int*)(arr->data + i*esz);
        int* ptr1 = (int*)(arr->data + j*esz);
        esz /= sizeof(int);
        for( size_t k = 0; k < esz; k++ ) {
            int t0 = ptr0[k], t1 = ptr1[k];
            ptr0[k] = t1; ptr1[k] = t0;
        }
    } else {
        char* ptr0 = arr->data + i*esz;
        char* ptr1 = arr->data + j*esz;
        for( size_t k = 0; k < esz; k++ ) {
            char t0 = ptr0[k], t1 = ptr1[k];
            ptr0[k] = t1; ptr1[k] = t0;
        }
    }
}
