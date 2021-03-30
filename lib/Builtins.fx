/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtins import *" directive in the beginning */

val __ficus_major__ : int = @ccode { FX_VERSION_MAJOR }
val __ficus_minor__ : int = @ccode { FX_VERSION_MINOR }
val __ficus_patchlevel__ : int = @ccode { FX_VERSION_PATCH }
val __ficus_git_commit__ : string = @ccode { FX_MAKE_STR(FX_GIT_COMMIT) }

// __ficus_version__, as a tuple, can be easily compared with a specific version, e.g.
// if __ficus_version__ >= (1, 0, 0) {...}
val __ficus_version__ = (__ficus_major__, __ficus_minor__, __ficus_patchlevel__)
val __ficus_version_str__ = f"{__ficus_major__}.{__ficus_minor__}.{__ficus_patchlevel__}"

exception ASCIIError
exception AssertError
exception BadArgError
exception Break
exception DimError
exception DivByZeroError
exception Fail: string
exception FileOpenError
exception IOError
exception NoMatchError
exception NotFoundError
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

fun assert(f: bool) = if !f {throw AssertError}

fun ignore(_: 't) {}

// 't?, int? etc. can be used instead of 't option, int option ...
object type 't option = None | Some: 't

type byte = uint8

fun value_or(x: 't?, defval: 't) = match x { | Some(x) => x | _ => defval }
fun value(x: 't?) = match x { | Some(x) => x | _ => throw OptionError }
fun isnone(x: 't?) { | Some _ => false | _ => true }
fun issome(x: 't?) { | Some _ => true | _ => false }

fun __is_scalar__(x: 't) = false
fun __is_scalar__(x: int) = true
fun __is_scalar__(x: int8) = true
fun __is_scalar__(x: uint8) = true
fun __is_scalar__(x: int16) = true
fun __is_scalar__(x: uint16) = true
fun __is_scalar__(x: int32) = true
fun __is_scalar__(x: uint32) = true
fun __is_scalar__(x: int64) = true
fun __is_scalar__(x: uint64) = true
fun __is_scalar__(x: float) = true
fun __is_scalar__(x: double) = true
fun __is_scalar__(x: char) = true
fun __is_scalar__(x: bool) = true

operator != (a: 't, b: 't): bool = !(a == b)
operator < (a: 't, b: 't): bool = a <=> b < 0
operator > (a: 't, b: 't): bool = a <=> b > 0
operator >= (a: 't, b: 't): bool = a <=> b >= 0
operator <= (a: 't, b: 't): bool = a <=> b <= 0

@pure @nothrow fun length(s: string): int = @ccode { return s->length }
@pure @nothrow fun length(l: 't list): int = @ccode { return fx_list_length(l) }

@pure fun join(sep: string, strs:string []): string = @ccode
{
    return fx_strjoin(0, 0, sep, (fx_str_t*)strs->data,
                    strs->dim[0].size, fx_result);
}

@pure fun join_embrace(begin: string, end: string,
        sep: string, strs:string []): string = @ccode
{
    return fx_strjoin(begin, end, sep,
        (fx_str_t*)strs->data,
        strs->dim[0].size, fx_result);
}

fun join_embrace(begin: string, end: string,
                 sep: string, strs: string list) =
    join_embrace(begin, end, sep, array(strs))

fun join(sep: string, strs: string list) =
    join(sep, [| for s <- strs {s} |])

operator + (a: string, b: string): string = @ccode
{
    fx_str_t s[] = {*a, *b};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: string, b: char): string = @ccode
{
    fx_str_t s[] = {*a, {0, &b, 1}};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: char, b: string): string = @ccode
{
    fx_str_t s[] = {{0, &a, 1}, *b};
    return fx_strjoin(0, 0, 0, s, 2, fx_result);
}

operator + (a: char, b: char): string = @ccode
{
    char_ cc[] = {a, b};
    return fx_make_str(cc, 2, fx_result);
}

operator + (l1: 't list, l2: 't list)
{
    @nothrow fun link2(l1: 't list, l2: 't list): 't list = @ccode { fx_link_lists(l1, l2, fx_result) }
    match (l1, l2) {
        | ([], _) => l2
        | (_, []) => l1
        | _ => link2([: for x <- l1 {x} :], l2)
    }
}

@pure fun string(a: exn): string = @ccode { return fx_exn_to_string(a, fx_result) }
@pure fun string(a: cptr): string = @ccode
{
    char buf[64];
    sprintf(buf, "cptr<%p: %p>", a, a->ptr);
    return fx_ascii2str(buf, -1, fx_result);
}
fun string(a: bool) = if a {"true"} else {"false"}
@pure fun string(a: int): string = @ccode  { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint8): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int8): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint16): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int16): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint32): string = @ccode { return fx_itoa(a, true, fx_result) }
@pure fun string(a: int32): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(a: uint64): string = @ccode { return fx_itoa((int64_t)a, true, fx_result) }
@pure fun string(a: int64): string = @ccode { return fx_itoa(a, false, fx_result) }
@pure fun string(c: char): string = @ccode { return fx_make_str(&c, 1, fx_result) }
@pure fun string(a: float): string = @ccode
{
    char buf[32];
    sprintf(buf, (a == (int)a ? "%.1f" : "%.8g"), a);
    return fx_ascii2str(buf, -1, fx_result);
}
@pure fun string(a: double): string = @ccode
{
    char buf[32];
    sprintf(buf, (a == (int)a ? "%.1f" : "%.16g"), a);
    return fx_ascii2str(buf, -1, fx_result);
}
fun string(a: string) = a

fun string(a: 't?) {
    | Some(a) => "Some(" + repr(a) + ")"
    | _ => "None"
}

fun ord(c: char) = (c :> int)
fun chr(i: int) = (i :> char)

fun odd(i: 't) = i % 2 != 0
fun odd(i: int64) = i % 2 != 0L
fun odd(i: uint64) = i % 2u64 != 0UL
fun even(i: 't) = i % 2 == 0
fun even(i: int64) = i % 2 == 0L
fun even(i: uint64) = i % 2u64 == 0UL

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

fun string(t: (...)) = join_embrace("(", ")", ", ", [| for x <- t {repr(x)} |])
fun string(r: {...}) = join_embrace("{", "}", ", ", [| for (n, x) <- r {n+"="+repr(x)} |])
fun string(a: 't ref) = "ref(" + repr(*a) + ")"

fun string(a: 't [])
{
    join_embrace("[", "]", ", ", [| for x <- a {repr(x)} |])
}

fun string(a: 't [,])
{
    val (m, n) = size(a)
    val rows = [| for i <- 0:m {
        val elems = [| for j <- 0:n {repr(a[i, j])} |]
        join(", ", elems)
    } |]
    join_embrace("[", "]", ";\n", rows)
}

fun string(l: 't list) = join_embrace("[", "]", ", ", [| for x <- l {repr(x)} |])

@pure fun string(a: char []): string = @ccode {
    return fx_make_str((char_*)a->data, a->dim[0].size, fx_result);
}

@pure operator * (c: char, n: int): string = @ccode
{
    int fx_status = fx_make_str(0, n, fx_result);
    if (fx_status >= 0) {
        for( int_ i = 0; i < n; i++ )
            fx_result->data[i] = c;
    }
    return fx_status;
}

@pure operator * (s: string, n: int): string = @ccode
{
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
    //a === b ||
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

operator == (a: 't [+], b: 't [+]): bool =
    size(a) == size(b) &&
    all(for xa <- a, xb <- b {xa == xb})

operator <=> (a: 't [], b: 't []): int
{
    val na = size(a), nb = size(b)
    val n = min(na, nb)
    var d = 0
    for i <- 0:n {
        d = xa <=> xb
        if d != 0 {break}
    }
    if d != 0 {d} else {na <=> nb}
}

operator == (a: (...), b: (...)) =
    fold f=true for aj <- a, bj <- b {if aj == bj {f} else {false}}
operator <=> (a: (...), b: (...)) =
    fold d=0 for aj <- a, bj <- b {if d != 0 {d} else {aj <=> bj}}
operator == (a: {...}, b: {...}) =
    fold f=true for (_, aj) <- a, (_, bj) <- b {if aj == bj {f} else {false}}
operator <=> (a: {...}, b: {...}) =
    fold d=0 for (_, aj) <- a, (_, bj) <- b {if d != 0 {d} else {aj <=> bj}}

operator + (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x + y} |]
operator - (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x - y} |]
operator .* (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .* y} |]
operator ./ (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x ./ y} |]
operator .% (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .% y} |]
operator .** (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .** y} |]
operator & (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x & y} |]
operator | (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x | y} |]
operator ^ (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x ^ y} |]

operator ' (a: 't [,])
{
    val (m, n) = size(a)
    [| for j <- 0:n for i <- 0:m {a[i, j]} |]
}

operator ' (a: 't [])
{
    val n = size(a)
    [| for j <- 0:n for i <- 0:1 {a[j]} |]
}

// matrix product: not very efficient and is put here for now just as a placeholder
operator * (a: 't [,], b: 't [,])
{
    val (ma, na) = size(a), (mb, nb) = size(b)
    assert(na == mb)
    val c = array((ma, nb), (0 :> 't))

    if ma*na*nb < (1<<20)
    {
        for i <- 0:ma for k <- 0:na {
            val alpha = a[i, k]
            for j <- 0:nb {
                c[i, j] += alpha*b[k, j]
            }
        }
    }
    else
    {
        @parallel for i <- 0:ma for k <- 0:na {
            val alpha = a[i, k]
            for j <- 0:nb {
                c[i, j] += alpha*b[k, j]
            }
        }
    }
    c
}

fun row2matrix(a: 't [])
{
    val n = size(a)
    [| for i <- 0:1 for j <- 0:n {a[j]} |]
}

operator * (a: 't [], b: 't [,]) = row2matrix(a)*b
operator * (a: 't [,], b: 't []) = a*row2matrix(b)

operator * (a: 't [+], alpha: 't) = [| for x <- a {x*alpha} |]
operator * (alpha: 't, a: 't [+]) = [| for x <- a {x*alpha} |]

fun diag(d: 't[])
{
    val n = size(a)
    val a = array((n, n), (0 :> 't))
    for i <- 0:n {a[i, i] = d[i]}
    a
}

fun diag(n: int, d: 't)
{
    val a = array((n, n), (0 :> 't))
    if d != (0 :> 't) {
        for i <- 0:n {a[i, i] = d}
    }
    a
}

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

operator + (a: (...), b: (...)) = (for aj <- a, bj <- b {aj + bj})
operator - (a: (...), b: (...)) = (for aj <- a, bj <- b {aj - bj})
operator .* (a: (...), b: (...)) = (for aj <- a, bj <- b {aj .* bj})
operator ./ (a: (...), b: (...)) = (for aj <- a, bj <- b {aj ./ bj})
operator | (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj | bj})
operator & (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj & bj})
operator ^ (a: ('t...), b: ('t...)): ('t...) = (for aj <- a, bj <- b {aj ^ bj})

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

operator .<=> (a: 't [+], b: 't [+]): int [+] =
    [| for x <- a, y <- b {x <=> y} |]
operator .== (a: 't [+], b: 't [+]): bool [+] =
    [| for x <- a, y <- b {x == y} |]
operator .!= (a: 't [+], b: 't [+]): bool [+] =
    [| for x <- a, y <- b {!(x == y)} |]
operator .< (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x < y} |]
operator .<= (a: 't [+], b: 't [+]): bool [+] =
    [| for x <- a, y <- b {!(y < x)} |]
operator .> (a: 't [+], b: 't [+]): bool [+] =
    [| for x <- a, y <- b {y < x} |]
operator .>= (a: 't [+], b: 't [+]): bool [+] =
    [| for x <- a, y <- b {!(x < y)} |]

@pure @nothrow operator == (a: string, b: string): bool = @ccode { return fx_streq(a, b); }

// [TODO] implement more clever string comparison operation
@pure @nothrow operator <=> (a: string, b: string): int = @ccode
{
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

// this is pseudo-function that is treated specially by the compiler
@pure @nothrow fun __eq_variants__(a: 't, b: 't): bool = a.__tag__ == b.__tag__

// operator === checks whether a and b represent not just equal, but the same object.
// For many of the types it's should be enough to check "a == b" at C level.
// But for some other types it may be not enough.
/*@pure @nothrow operator === (a: 't, b: 't): bool = @ccode {return a == b}
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
}*/

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

@pure @nothrow fun sat_uint8(i: int): uint8 = @ccode
{ return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255); }

@pure @nothrow fun sat_uint8(f: float): uint8 = @ccode
{
    int i = fx_roundf2i(f);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);
}

@pure @nothrow fun sat_uint8(d: double): uint8 = @ccode
{
    int_ i = fx_round2I(d);
    return (unsigned char)((i & ~255) != 0 ? i : i < 0 ? 0 : 255);
}

@pure @nothrow fun sat_int8(i: int): int8 = @ccode
{ return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127); }

@pure @nothrow fun sat_int8(f: float): int8 = @ccode
{
    int i = fx_roundf2i(f);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);
}

@pure @nothrow fun sat_int8(d: double): int8 = @ccode
{
    int_ i = fx_round2I(d);
    return (signed char)(((i + 128) & ~255) != 0 ? i : i < 0 ? -128 : 127);
}

@pure @nothrow fun sat_uint16(i: int): uint16 = @ccode
{
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_uint16(f: float): uint16 = @ccode
{
    int i = fx_roundf2i(f);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_uint16(d: double): uint16 = @ccode
{
    int_ i = fx_round2I(d);
    return (unsigned short)((i & ~65535) != 0 ? i : i < 0 ? 0 : 65535);
}

@pure @nothrow fun sat_int16(i: int): int16 = @ccode
{
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);
}

@pure @nothrow fun sat_int16(f: float): int16 = @ccode
{
    int i = fx_roundf2i(f);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);
}

@pure @nothrow fun sat_int16(d: double): int16 = @ccode
{
    int_ i = fx_round2I(d);
    return (short)(((i+32768) & ~65535) != 0 ? i : i < 0 ? -32768 : 32767);
}

fun sat_uint8(x: ('t...)) = (for xj <- x {sat_uint8(xj)})
fun sat_int8(x: ('t...)) = (for xj <- x {sat_int8(xj)})
fun sat_uint16(x: ('t...)) = (for xj <- x {sat_uint16(xj)})
fun sat_int16(x: ('t...)) = (for xj <- x {sat_int16(xj)})

// do not use lrint(x), since it's slow. and (int)round(x) is even slower
@pure @nothrow fun round(x: float): int = @ccode { return fx_roundf2I(x) }
@pure @nothrow fun round(x: double): int = @ccode { return fx_round2I(x) }
@pure @nothrow fun round(x: float, n: int): float = @ccode
{
    double scale =
        n ==  0 ? 1   : n == 1 ? 1e1 : n == 2 ? 1e2 : n == 3 ? 1e3 :
        n ==  4 ? 1e4 : n == 5 ? 1e5 : n == 6 ? 1e6 : n == 7 ? 1e7 : 1e8;
    return (float)(fx_round2I(x*scale)/scale);
}
@pure @nothrow fun round(x: double, n: int): double = @ccode
{
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
fun abs(a: 't) = if a >= (0 :> 't) {a} else {-a}
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
@nothrow fun print(a: float): void = @ccode { printf((a == (int)a ? "%.1f" : "%.8g"), a) }
@nothrow fun print(a: double): void = @ccode { printf((a == (int)a ? "%.1f" : "%.16g"), a) }
@nothrow fun print(a: cptr): void = @ccode { printf("cptr<%p: %p>", a, a->ptr) }
fun print(a: string) = print_string(a)
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
        if i < m-1 {print(";\n ")} else {print("]")}
    }
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

fun print(t: (...))
{
    print("(");
    for x@i <- t {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print(")")
}

/*fun print(r: {...})
{
    print("{")
    for (n, x)@i <- r {
        if i > 0 {print(", ")}
        print(n + "=");
        print_repr(x)
    }
    print("}")
}*/

fun print(a: 't ref) {
    print("ref("); print_repr(*a); print(")")
}

fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }

fun list(a: 't []): 't list = [: for x <- a {x} :]
fun list(s: string) = [: for c <- s {c} :]

fun array(): 't [] = [| for i<-0:0 {(None : 't?).value()} |]
fun array(n: int, x: 't) = [| for i <- 0:n {x} |]
fun array((m: int, n: int), x: 't) = [| for i <- 0:m for j <- 0:n {x} |]
fun array((m: int, n: int, l: int), x: 't) = [| for i <- 0:m for j <- 0:n for k <- 0:l {x} |]
fun array(l: 't list): 't [] = [| for x <- l {x} |]

fun copy(a: 't [+]) = [| for x <- a {x} |]

@pure @nothrow fun size(a: 't []): int = @ccode { return a->dim[0].size }
@pure @nothrow fun size(a: 't [,]): (int, int) = @ccode
{
    fx_result->t0=a->dim[0].size;
    fx_result->t1=a->dim[1].size
}

@pure @nothrow fun size(a: 't [,,]): (int, int, int) = @ccode
{
    fx_result->t0=a->dim[0].size;
    fx_result->t1=a->dim[1].size;
    fx_result->t2=a->dim[2].size
}

fun sort(arr: 't [], lt: ('t, 't) -> bool)
{
    @nothrow fun swap(arr: 't [], i: int, j: int): void = @ccode
    {
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

    fun qsort_(lo: int, hi: int) =
        if lo+1 < hi {
            val m = (lo+hi)/2
            val a = arr[lo], b = arr[m], p = arr[hi]
            val p =
                if lt(a, b) {
                    if lt(b, p) {arr[m]=p; b} else if lt(a, p) {p} else {arr[lo]=p; a}
                } else {
                    if lt(a, p) {arr[lo]=p; a} else if lt(b, p) {p} else {arr[m]=p; b}
                }
            val i0 =
                if __is_scalar__(p) {
                    fold i0 = lo for j <- lo:hi {
                        val b = arr[j]
                        if lt(b, p) {
                            val a = arr[i0]
                            arr[i0] = b; arr[j] = a;
                            i0 + 1
                        } else {i0}
                    }
                } else {
                    fold i0 = lo for j <- lo:hi {
                        if lt(arr[j], p) {
                            swap(arr, i0, j)
                            i0 + 1
                        } else {i0}
                    }
                }
            val a = arr[i0]
            arr[hi] = a; arr[i0] = p
            var i1 = hi
            for j <- i0:hi {
                if lt(p, arr[j+1]) {i1=j; break}
            }
            // do the longest half sorting via tail recursion to save stack space
            if i0 - lo < hi - i1 {
                qsort_(lo, i0-1)
                qsort_(i1+1, hi)
            } else {
                qsort_(lo, i0-1)
                qsort_(i1+1, hi)
            }
        } else if lo < hi {
            val a = arr[lo], b = arr[hi]
            if lt(b, a) {
                arr[hi] = a
                arr[lo] = b
            }
        }

    qsort_(0, size(arr)-1)
}

fun new_uniform_rng(seed: uint64) {
    var state = if seed != 0UL {seed} else {0xffffffffUL}
    fun (a: int, b: int) {
        state = (state :> uint32) * 4197999714UL + (state >> 32)
        val (a, b) = (min(a, b), max(a, b))
        val diff = b - a
        val x = (state :> uint32) % diff + a
        (x :> int)
    }
}

@ccode {

typedef uint64_t hash_t;
#define FNV_1A_PRIME 1099511628211ULL
#define FNV_1A_OFFSET 14695981039346656037ULL

}

type hash_t = uint64
val FNV_1A_PRIME: hash_t = 1099511628211UL
val FNV_1A_OFFSET: hash_t = 14695981039346656037UL

fun hash(x: int) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: int32) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: uint32) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: int8) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: uint8) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: int16) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: uint16) = uint64(x) ^ FNV_1A_OFFSET
fun hash(x: bool) = uint64(x) ^ FNV_1A_OFFSET
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

/*fun hash(x: (...)): hash_t =
    fold h = FNV_1A_OFFSET for xj <- x {
        val h = h ^ hash(xj)
        h * FNV_1A_PRIME
    }

fun hash(x: {...}): hash_t =
    fold h = FNV_1A_OFFSET for (_, xj) <- x {
        val h = h ^ hash(xj)
        h * FNV_1A_PRIME
    }
*/