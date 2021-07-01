/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on arrays

fun total(a: 't [+]) = fold p = 1 for szj <- size(a) {p*szj}
fun total(a: 't []) = size(a)
@nothrow fun empty(a: 't [+]):bool = @ccode
{
    if (!a->data || a->ndims == 0) return true;
    for(int i = 0; i < a->ndims; i++)
        if (a->dim[i].size == 0)
            return true;
    return false;
}

fun __negate__(a: 't [+]) = [| for x <- a {-x} |]

operator .+ (a: 'ta [+], b: 'tb) =
    [| for x <- a {x .+ b} |]
operator .- (a: 'ta [+], b: 'tb) =
    [| for x <- a {x .- b} |]
operator .* (a: 'ta [+], b: 'tb) =
    [| for x <- a {x .* b} |]
operator ./ (a: 'ta [+], b: 'tb) =
    [| for x <- a {x ./ b} |]
operator .% (a: 'ta [+], b: 'tb) =
    [| for x <- a {x .% b} |]
operator .** (a: 'ta [+], b: 'tb) =
    [| for x <- a {x .** b} |]

operator .+ (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a .+ y} |]
operator .- (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a .- y} |]
operator .* (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a .* y} |]
operator ./ (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a ./ y} |]
operator .% (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a .% y} |]
operator .** (a: 'ta, b: 'tb [+]) =
    [| for y <- b {a .** y} |]

operator + (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x + y} |]
operator - (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x - y} |]
operator .+ (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x + y} |]
operator .- (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x - y} |]
operator .* (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .* y} |]
operator ./ (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x ./ y} |]
operator .% (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .% y} |]
operator .** (a: 'ta [+], b: 'tb [+]) =
    [| for x <- a, y <- b {x .** y} |]

operator & (a: 't, b: 't [+]) =
    [| for y <- b {a & y} |]
operator | (a: 't, b: 't [+]) =
    [| for y <- b {a | y} |]
operator ^ (a: 't, b: 't [+]) =
    [| for y <- b {a ^ y} |]
operator & (a: 't [+], b: 't) =
    [| for x <- a {x & b} |]
operator | (a: 't [+], b: 't) =
    [| for x <- a {x | b} |]
operator ^ (a: 't [+], b: 't) =
    [| for x <- a {x ^ b} |]

operator & (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x & y} |]
operator | (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x | y} |]
operator ^ (a: 't [+], b: 't [+]) =
    [| for x <- a, y <- b {x ^ y} |]

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

fun sum(a: 't [+]) =
    fold s = ((0 :> 't) :> double) for aj <- a {s + aj}
fun mean(a: 't [+]) = sum(a)/(max(total(a), 1) :> double)

fun normInf(a: 't [+]) =
    fold s = 0. for aj <- a {max(s, double(abs(aj)))}

fun normL1(a: 't [+]) =
    fold s = 0. for aj <- a {s + abs(aj)}

fun normL2(a: 't [+]) =
    sqrt(fold s = 0. for aj <- a {s + double(aj)*aj})

fun normInf(a: 't [+], b: 't [+]) =
    fold s = 0. for aj <- a, bj <- b {val d = aj - bj; max(s, double(abs(d)))}

fun normL1(a: 't [+], b: 't [+]) =
    fold s = 0. for aj <- a, bj <- b {val d = aj - bj; s + abs(d)}

fun normL2(a: 't [+], b: 't [+]) =
    sqrt(fold s = 0. for aj <- a, bj <- b {val d = aj - bj; s + double(d)*d})

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

fun random(rng: RNG, size: int, a: 't, b: 't) =
    [| for i <- 0:size {rng.uniform(a, b)} |]

fun random(rng: RNG, size: (int, int), a: 't, b: 't) =
    [| for i <- 0:size.0 for j <- 0:size.1 {rng.uniform(a, b)} |]

fun random(rng: RNG, size: (int, int, int), a: 't, b: 't) =
    [| for i <- 0:size.0 for j <- 0:size.1 for k <- 0:size.2 {rng.uniform(a, b)} |]

fun random(rng: RNG, size: (int, int, int, int), a: 't, b: 't) =
    [| for i <- 0:size.0 for j <- 0:size.1
       for k <- 0:size.2 for l <- 0:size.3 {rng.uniform(a, b)} |]

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

fun trace(a: 't [,]): double
{
    val (m, n) = size(a)
    fold s = 0. for i <- 0:min(m, n) {s + a[i, i]}
}