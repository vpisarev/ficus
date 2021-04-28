/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on arrays

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
