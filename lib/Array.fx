/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on arrays
fun mkrange(n: int): int [] = [for i <- 0:n {i}]
fun mkrange(a: int, b: int): int [] = [for i <- a:b {i}]
fun mkrange(a: int, b: int, delta: int): int [] = [for i <- a:b:delta {i}]

fun channels[T](a: (T*2) [+]): int = 2
fun channels[T](a: (T*3) [+]): int = 3
fun channels[T](a: (T*4) [+]): int = 4
fun channels[T](a: (T*5) [+]): int = 5

fun length[T](a: T []): int = __intrin_size__(a)
fun size[T](a: T []): int = __intrin_size__(a)
fun size[T](a: T [,]): (int, int) = (__intrin_size__(a, 0), __intrin_size__(a, 1))
fun size[T](a: T [,,]): (int, int, int) = (__intrin_size__(a, 0), __intrin_size__(a, 1), __intrin_size__(a, 2))
fun size[T](a: T [,,,]): (int, int, int, int) = (__intrin_size__(a, 0), __intrin_size__(a, 1),
                         __intrin_size__(a, 2), __intrin_size__(a, 3))

fun total[T](a: T [+]): int = fold p = 1 for szj <- size(a) { p*= szj }
fun total[T](a: T []): int = size(a)

@nothrow fun empty[T](a: T [+]):bool
@ccode {
    if (!a->data || a->ndims == 0) return true;
    for(int i = 0; i < a->ndims; i++)
        if (a->dim[i].size == 0)
            return true;
    return false;
}

fun copy[T](a: T [+]): T [+] = [for x <- a {x}]

fun reshape[T](a: T [+], size: (int*2)): T [,]
@ccode {
    return fx_reshape_arr(a, 2, &size->t0, 1, 1, fx_result);
}
fun reshape[T](a: T [+], s0: int, s1: int): T [,] = reshape(a, (s0, s1))

fun reshape[T](a: T [+], size: (int*3)): T [,,]
@ccode {
    return fx_reshape_arr(a, 3, &size->t0, 1, 1, fx_result);
}
fun reshape[T](a: T [+], s0: int, s1: int, s2: int): T [,,] = reshape(a, (s0, s1, s2))

fun reshape[T](a: T [+], size: (int*4)): T [,,,]
@ccode {
    return fx_reshape_arr(a, 4, &size->t0, 1, 1, fx_result);
}
fun reshape[T](a: T [+], s0: int, s1: int, s2: int, s3: int): T [,,,] = reshape(a, (s0, s1, s2, s3))

fun reshape_multichan_[T](a: (T ...)[+], nchannels: int, size: (int*4)): T [,,,]
@ccode {
    return fx_reshape_arr(a, 4, &size->t0, (int)nchannels, 1, fx_result);
}
fun reshape_multichan[T](a: (T ...) [+], size: (int*4)): T [,,,] = reshape_multichan_(a, channels(a), size)
fun reshape_multichan_c3[T](a: T [+], size: (int*2)): (T*3) [,]
@ccode {
    return fx_reshape_arr(a, 2, &size->t0, 1, 3, fx_result);
}
fun reshape_multichan_c3[T](a: T [+], s0: int, s1: int): (T*3) [,] = reshape_multichan_c3(a, (s0, s1))

fun __negate__[T](a: T [+]): T [+] = [for x <- a {-x}]

fun map[T, Tr](arr: T [+], f: T -> Tr): Tr [+] = [for x <- arr {f(x)}]
fun assoc_opt[T1, T2](arr: (T1, T2) [], key: T1): T2? =
    match find_opt(for (a, b) <- arr {a == key}) {
    | Some((a, b)) => Some(b)
    | _ => None
    }
fun assoc[T1, T2](arr: (T1, T2) [], key: T1): T2 =
    match find_opt(for (a, b) <- arr {a == key}) {
    | Some((a, b)) => b
    | _ => throw NotFoundError
    }

fun sat_uint8[T](a: T [+]): uint8 [+] = [for x <- a {sat_uint8(x)}]
fun sat_int8[T](a: T [+]): int8 [+] = [for x <- a {sat_int8(x)}]
fun sat_uint16[T](a: T [+]): uint16 [+] = [for x <- a {sat_uint16(x)}]
fun sat_int16[T](a: T [+]): int16 [+] = [for x <- a {sat_int16(x)}]

operator .+ [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x .+ b}]
operator .- [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x .- b}]
operator .* [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x .* b}]
operator ./ [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x ./ b}]
operator .% [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x .% b}]
operator .** [Ta, Tb, Tr](a: Ta [+], b: Tb): Tr [+] =
    [for x <- a {x .** b}]

operator .+ [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a .+ y}]
operator .- [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a .- y}]
operator .* [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a .* y}]
operator ./ [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a ./ y}]
operator .% [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a .% y}]
operator .** [Ta, Tb, Tr](a: Ta, b: Tb [+]): Tr [+] =
    [for y <- b {a .** y}]

operator + [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x + y}]
operator - [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x - y}]
operator .+ [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x + y}]
operator .- [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x - y}]
operator .* [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x .* y}]
operator ./ [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x ./ y}]
operator .% [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x .% y}]
operator .** [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): Tr [+] =
    [for x <- a, y <- b {x .** y}]

operator += [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x + b} }
operator -= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x - b} }
operator .*= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x * b} }
operator ./= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x / b} }
operator .%= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x % b} }

operator += [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x + y} }
operator -= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x - y} }
operator .*= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x * y} }
operator ./= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x / y} }
operator .%= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x % y} }

operator & [T](a: T, b: T [+]): T [+] =
    [for y <- b {a & y}]
operator | [T](a: T, b: T [+]): T [+] =
    [for y <- b {a | y}]
operator ^ [T](a: T, b: T [+]): T [+] =
    [for y <- b {a ^ y}]
operator & [T](a: T [+], b: T): T [+] =
    [for x <- a {x & b}]
operator | [T](a: T [+], b: T): T [+] =
    [for x <- a {x | b}]
operator ^ [T](a: T [+], b: T): T [+] =
    [for x <- a {x ^ b}]

operator & [T](a: T [+], b: T [+]): T [+] =
    [for x <- a, y <- b {x & y}]
operator | [T](a: T [+], b: T [+]): T [+] =
    [for x <- a, y <- b {x | y}]
operator ^ [T](a: T [+], b: T [+]): T [+] =
    [for x <- a, y <- b {x ^ y}]

operator &= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x & b} }
operator |= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x | b} }
operator ^= [Ta, Tb, Tr](a: Ta [+], b: Tb): void { for x@idx <- a { a[idx] = x ^ b} }

operator &= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x & y} }
operator |= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x | y} }
operator ^= [Ta, Tb, Tr](a: Ta [+], b: Tb [+]): void { for x@idx <- a, y <- b { a[idx] = x ^ y} }

operator .<=> [T](a: T [+], b: T [+]): int [+] =
    [for x <- a, y <- b {x <=> y}]
operator .== [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {x == y}]
operator .!= [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {!(x == y)}]
operator .< [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {x < y}]
operator .<= [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {!(y < x)}]
operator .> [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {y < x}]
operator .>= [T](a: T [+], b: T [+]): bool [+] =
    [for x <- a, y <- b {!(x < y)}]

operator .<=> [T](x: T, b: T [+]): int [+] =
    [for y <- b {x <=> y}]
operator .== [T](x: T, b: T [+]): bool [+] =
    [for y <- b {x == y}]
operator .!= [T](x: T, b: T [+]): bool [+] =
    [for y <- b {!(x == y)}]
operator .< [T](x: T, b: T [+]): bool [+] =
    [for y <- b {x < y}]
operator .<= [T](x: T, b: T [+]): bool [+] =
    [for y <- b {!(y < x)}]
operator .> [T](x: T, b: T [+]): bool [+] =
    [for y <- b {y < x}]
operator .>= [T](x: T, b: T [+]): bool [+] =
    [for y <- b {!(x < y)}]

operator .<=> [T](a: T [+], y: T): int [+] =
    [for x <- a {x <=> y}]
operator .== [T](a: T [+], y: T): bool [+] =
    [for x <- a {x == y}]
operator .!= [T](a: T [+], y: T): bool [+] =
    [for x <- a {!(x == y)}]
operator .< [T](a: T [+], y: T): bool [+] =
    [for x <- a {x < y}]
operator .<= [T](a: T [+], y: T): bool [+] =
    [for x <- a {!(y < x)}]
operator .> [T](a: T [+], y: T): bool [+] =
    [for x <- a {y < x}]
operator .>= [T](a: T [+], y: T): bool [+] =
    [for x <- a {!(x < y)}]

fun min[T](a: T [+], b: T [+]): T [+] =
    [for x <- a, y <- b {min(x, y)}]
fun max[T](a: T [+], b: T [+]): T [+] =
    [for x <- a, y <- b {max(x, y)}]

fun min[T](x: T, b: T [+]): T [+] =
    [for y <- b {min(x, y)}]
fun max[T](x: T, b: T [+]): T [+] =
    [for y <- b {max(x, y)}]

fun min[T](a: T [+], y: T): T [+] =
    [for x <- a {min(x, y)}]
fun max[T](a: T [+], y: T): T [+] =
    [for x <- a {max(x, y)}]

fun clip[T](a: T [+], minv: T, maxv: T): T [+] =
    [for x <- a {min(max(x, minv), maxv)}]
fun clip[T](a: T [+], minv_arr: T [+], maxv_arr: T [+]): T [+] =
    [for x <- a, minv <- minv_arr, maxv <- maxv_arr {min(max(x, minv), maxv)}]

fun sum[T](a: T [+]): double =
    fold s = ((0 :> T) :> double) for aj <- a {s += aj}

fun sum[T, S](a: T [+], v0: S): S =
    fold s = v0 for aj <- a {s += aj}

fun product[T, S](a: T [+], v0: S): S =
    fold p = v0 for aj <- a {p *= aj}

fun mean[T](a: T [+]): double = sum(a)/(max(total(a), 1) :> double)

fun normInf[T](a: T [+]): T =
    fold s = normInf(0 :> T) for aj <- a {s = max(s, normInf(aj))}

fun normL1[T](a: T [+]): double =
    fold s = 0. for aj <- a {s += normL1(aj)}

fun normL2sqr[T](a: T [+]): double =
    fold s = 0. for aj <- a {s += normL2sqr(aj)}

fun normL2[T](a: T [+]): double = sqrt(normL2sqr(a))

fun normInf[T](a: T [+], b: T [+]): T =
    fold s = normInf(0 :> T) for aj <- a, bj <- b {s = max(s, normInf(aj, bj))}

fun normL1[T](a: T [+], b: T [+]): double =
    fold s = 0. for aj <- a, bj <- b {s += normL1(aj, bj)}

fun normL2sqr[T](a: T [+], b: T [+]): double =
    fold s = 0. for aj <- a, bj <- b {s += normL2sqr(aj, bj)}

fun normL2[T](a: T [+], b: T [+]): double = sqrt(normL2sqr(a, b))

fun minindex[T](a: T []): (T, int) =
    if a == [] {
        (0 :> T, -1)
    } else {
        fold minv = a[0], mini = 0 for x@i <- a {
            if x < minv {
                minv = x
                mini = i
            }
        }
    }

fun maxindex[T](a: T []): (T, int) =
    if a == [] {
        (0 :> T, -1)
    } else {
        fold maxv = a[0], maxi = 0 for x@i <- a {
            if x > maxv {
                maxv = x
                maxi = i
            }
        }
    }

operator ' [T](a: T [,]): T [,]
{
    val (m, n) = size(a)
    [for j <- 0:n for i <- 0:m {a[i, j]}]
}

operator ' [T](a: T []): T [,]
{
    val n = size(a)
    [for j <- 0:n for i <- 0:1 {a[j]}]
}

// matrix product: not very efficient and is put here for now just as a placeholder
operator * [T](a: T [,], b: T [,]): T [,]
{
    val (ma, na) = size(a), (mb, nb) = size(b)
    assert(na == mb)
    val c = array((ma, nb), (0 :> T))

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

operator * (a: float [,], b: float [,]): float [,] = __intrin_gemm__(a, false, 0, -1, 1, 0, -1, 1, b, false, 0, -1, 1, 0, -1, 1)
operator * (a: double [,], b: double [,]): double [,] = __intrin_gemm__(a, false, 0, -1, 1, 0, -1, 1, b, false, 0, -1, 1, 0, -1, 1)

operator *= [T](a: T [,], b: T [,]): void {
    val temp = a*b
    a[:,:] = temp
}

fun row2matrix[T](a: T []): T [,]
{
    val n = size(a)
    [for i <- 0:1 for j <- 0:n {a[j]}]
}

operator * [T](a: T [], b: T [,]): T [,] = row2matrix(a)*b
operator * [T](a: T [,], b: T []): T [,] = a*row2matrix(b)

operator * [T](a: T [+], alpha: T): T [+] = [for x <- a {x*alpha}]
operator * [T](alpha: T, a: T [+]): T [+] = [for x <- a {x*alpha}]

fun diag[T](d: T[]): T [,]
{
    val n = size(d)
    val a = array((n, n), (0 :> T))
    for i <- 0:n {a[i, i] = d[i]}
    a
}

fun diag[T](n: int, d: T): T [,]
{
    val a = array((n, n), (0 :> T))
    if d != (0 :> T) {
        for i <- 0:n {a[i, i] = d}
    }
    a
}

fun random[T](rng: RNG, size: int, a: T, b: T): T [] =
    [for i <- 0:size {rng.uniform(a, b)}]

fun random[T](rng: RNG, size: (int, int), a: T, b: T): T [,] =
    [for i <- 0:size.0 for j <- 0:size.1 {rng.uniform(a, b)}]

fun random[T](rng: RNG, size: (int, int, int), a: T, b: T): T [,,] =
    [for i <- 0:size.0 for j <- 0:size.1 for k <- 0:size.2 {rng.uniform(a, b)}]

fun random[T](rng: RNG, size: (int, int, int, int), a: T, b: T): T [,,,] =
    [for i <- 0:size.0 for j <- 0:size.1
       for k <- 0:size.2 for l <- 0:size.3 {rng.uniform(a, b)}]

fun shuffle[T](arr: T [], rng: RNG): void
{
    val n = size(arr)
    if n > 1 {
        if __is_scalar__(arr[0]) {
            for i <- 0:n {
                val j = rng.uniform(0, n-1)
                val t = arr[i]
                arr[i] = arr[j]
                arr[j] = t
            }
        } else {
            for i <- 0:n {
                val j = rng.uniform(0, n-1)
                _swap_(arr, i, j)
            }
        }
    }
}

fun sort[T](arr: T [], lt: (T, T) -> bool, ~prefix: int): void
{
    fun qsort_(lo: int, hi: int) {
        if lo+1 < hi {
            val m = (lo+hi)/2
            val a = arr[lo], b = arr[m], p = arr[hi]
            val p =
                if lt(a, b) {
                    if lt(b, p) {arr[m]=p; b} else if lt(a, p) {p} else {arr[lo]=p; a}
                } else {
                    if lt(a, p) {arr[lo]=p; a} else if lt(b, p) {p} else {arr[m]=p; b}
                }
            var i0 = lo
            if __is_scalar__(p) {
                for j <- lo:hi {
                    val b = arr[j]
                    if lt(b, p) {
                        val a = arr[i0]
                        arr[i0] = b; arr[j] = a;
                        i0 += 1
                    }
                }
            } else {
                for j <- lo:hi {
                    if lt(arr[j], p) {
                        _swap_(arr, i0, j)
                        i0 += 1
                    }
                }
            }
            val a = arr[i0]
            arr[hi] = a; arr[i0] = p
            var i1 = hi
            for j <- i0:hi {
                if lt(p, arr[j+1]) {i1=j; break}
            }
            // do the shortest half sorting via tail recursion to save stack space
            if i0 - lo < hi - i1 {
                if i1 < prefix { qsort_(i1+1, hi) }
                qsort_(lo, i0-1)
            } else {
                qsort_(lo, i0-1)
                if i1 < prefix { qsort_(i1+1, hi) }
            }
        } else if lo < hi {
            val a = arr[lo], b = arr[hi]
            if lt(b, a) {
                arr[hi] = a
                arr[lo] = b
            }
        }
    }

    qsort_(0, size(arr)-1)
}

fun sort[T](arr: T [], lt: (T, T) -> bool): void = sort(arr, lt, prefix=size(arr))

@ccode {
/*
    Linear system solution using QR factorization
    Converted from OpenCV implementation, here is the original copyright:
    ---
    This file is part of OpenCV project.
    It is subject to the license terms in the LICENSE file found in the top-level directory
    of this distribution and at http://opencv.org/license.html
    ---
*/
enum {
    FX_MATX_BUF = 1024,
};

#define FX_MATX_QR_EPS 1e-12

static int fx_decomp_qr(const double* A_, int_ astep, int_ m, int_ n, int_ k,
                        const double* b, int_ bstep, double* x, int_ xstep,
                        double* hFactors, double scale, double eps,
                        double* det_)
{
    int status = 1;
    double localbuf[FX_MATX_BUF], *buf = localbuf;
    double *A, *vl;
    int_ bufsize = m*n + m + n, nm = m < n ? m : n;

    if (det_) *det_ = 0.;
    if (bufsize > FX_MATX_BUF) {
        buf = (double*)fx_malloc(bufsize);
        if (!buf) return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    }
    A = buf;
    vl = buf + m*n;
    for(int i = 0; i < m; i++)
        memcpy(A + i*n, A_ + i*astep, n*sizeof(A[0]));
    astep = n;

    if (!hFactors)
        hFactors = vl + m;

    for (int_ l = 0; l < n; l++) {
        //generate vl
        int_ vlSize = m - l;
        double vlNorm = 0.;
        for (int_ i = 0; i < vlSize; i++) {
            vl[i] = A[(l + i)*astep + l];
            vlNorm += vl[i] * vl[i];
        }
        double tmpV = vl[0];
        vl[0] = vl[0] + (vl[0] < 0. ? -1 : vl[0] > 0.)*sqrt(vlNorm);
        vlNorm = sqrt(vlNorm + vl[0] * vl[0] - tmpV*tmpV);
        for (int_ i = 0; i < vlSize; i++)
            vl[i] /= vlNorm;
        //multiply A_l*vl
        for (int_ j = l; j < n; j++) {
            double v_lA = 0.;
            for (int_ i = l; i < m; i++)
                v_lA += vl[i - l] * A[i*astep + j];

            for (int_ i = l; i < m; i++)
                A[i*astep + j] -= 2 * vl[i - l] * v_lA;
        }

        //save vl and factors
        hFactors[l] = vl[0] * vl[0];
        for (int_ i = 1; i < vlSize; i++)
            A[(l + i)*astep + l] = vl[i] / vl[0];
    }

    if (x) {
        for(int_ i = 0; i < m; i++) {
            if(b)
                memcpy(x + i*xstep, b + i*bstep, k*sizeof(x[0]));
            else {
                memset(x + i*xstep, 0, k*sizeof(x[0]));
                x[i*xstep + i] = scale;
            }
        }
        //generate new rhs
        for (int_ l = 0; l < n; l++) {
            //unpack vl
            vl[0] = (double)1;
            for (int_ j = 1; j < m - l; j++)
                vl[j] = A[(j + l)*astep + l];

            //h_l*x
            for (int_ j = 0; j < k; j++) {
                double v_lB = (double)0;
                for (int_ i = l; i < m; i++)
                  v_lB += vl[i - l] * x[i*xstep + j];

                for (int_ i = l; i < m; i++)
                    x[i*xstep + j] -= 2 * vl[i - l] * v_lB * hFactors[l];
            }
        }
        //do back substitution
        for (int_ i = n - 1; i >= 0; i--) {
            for (int_ j = n - 1; j > i; j--) {
                double f = A[i*astep + j];
                for (int_ p = 0; p < k; p++)
                    x[i*xstep + p] -= x[j*xstep + p] * f;
            }
            if (fabs(A[i*astep + i]) < eps) {
                status = 0;
                break;
            } else {
                double f = A[i*astep + i];
                for (int_ p = 0; p < k; p++)
                    x[i*xstep + p] /= f;
            }
        }
    }

    if(det_) {
        double p = nm & 1 ? -1 : 1;
        for(int_ i = 0; i < nm; i++)
            p *= A[astep*i + i];
        *det_ = p;
    }

    if(buf != localbuf)
        fx_free(buf);

    return status;
}

}

operator \ (a: double [,], b: double [,]): double [,] =
@ccode
{
    int_ m = a->dim[0].size, n = a->dim[1].size, k = b->dim[1].size;
    int_ xsz[] = {m, k};
    int status, elemsz = (int)sizeof(double);
    if (m != n || m == 0 || m != b->dim[0].size || k == 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    status = fx_make_arr(2, xsz, elemsz, 0, 0, 0, fx_result);
    if(status >= 0)
        status = fx_decomp_qr(
            (double*)a->data, a->dim[0].step/elemsz, m, n, k,
            (double*)b->data, b->dim[0].step/elemsz,
            (double*)fx_result->data, fx_result->dim[0].step/elemsz,
            0, 1, FX_MATX_QR_EPS, 0);
    if(status < 0)
        return status;
    if(status == 0)
        memset(fx_result->data, 0, m*k*elemsz);
    return FX_OK;
}

operator \ (a: double [,], b: double []): double [] =
@ccode
{
    int_ m = a->dim[0].size, n = a->dim[1].size, k = 1;
    int_ xsz[] = {m, k};
    int status, elemsz = (int)sizeof(double);
    if (m != n || m == 0 || m != b->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    status = fx_make_arr(1, xsz, elemsz, 0, 0, 0, fx_result);
    if(status >= 0)
        status = fx_decomp_qr(
            (double*)a->data, a->dim[0].step/elemsz, m, n, k,
            (double*)b->data, 1,
            (double*)fx_result->data, 1,
            0, 1, FX_MATX_QR_EPS, 0);
    if(status < 0)
        return status;
    if(status == 0)
        memset(fx_result->data, 0, m*k*elemsz);
    return FX_OK;
}

operator \ (a: double [,], scale: double): double [,] =
@ccode
{
    int_ m = a->dim[0].size, n = a->dim[1].size, k = n;
    int_ xsz[] = {m, k};
    int status, elemsz = (int)sizeof(double);
    if (m != n || m == 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    status = fx_make_arr(2, xsz, elemsz, 0, 0, 0, fx_result);
    if(status >= 0)
        status = fx_decomp_qr(
            (double*)a->data, a->dim[0].step/elemsz, m, n, k, 0, 0,
            (double*)fx_result->data, fx_result->dim[0].step/elemsz,
            0, scale, FX_MATX_QR_EPS, 0);
    if(status < 0)
        return status;
    if(status == 0)
        memset(fx_result->data, 0, m*k*elemsz);
    return FX_OK;
}

operator \ (a: double [,], scale: int): double [,] = a\double(scale)

fun det(a: double [,]): double =
@ccode
{
    int_ m = a->dim[0].size, n = a->dim[1].size;
    int status, elemsz = (int)sizeof(double);
    if (m != n || m == 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    status = fx_decomp_qr(
        (double*)a->data, a->dim[0].step/elemsz, m, n, 1, 0, 0,
        0, 0, 0, 1, FX_MATX_QR_EPS, fx_result);
    return status < 0 ? status : FX_OK;
}

fun trace[T](a: T [,]): double
{
    val (m, n) = size(a)
    fold s = 0. for i <- 0:min(m, n) {s += a[i, i]}
}
