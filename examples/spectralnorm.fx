import Args, Math

fun A(i: int, j: int)
{
    val i = double(i), j = double(j)
    1. / ((i + j) * (i + j + 1) / 2. + i + 1)
}

fun Au(u: double[])
{
    val N = size(u)

    [for i <- 0:N {
        fold t = 0. for j <- 0:N {
            t + A(i, j) * u[j]
        }
    }]
}

fun Atu(u: double[])
{
    val N = size(u)

    [for i <- 0:N {
        fold t = 0. for j <- 0:N {
            t + A(j, i) * u[j]
        }
    }]
}

fun AtAu(u: double[]) = Atu(Au(u))

fun spectralnorm(n: int)
{
    val fold (u, v) = (array(n, 1.), array(0, 0.))
        for i <- 0:10 { val v = AtAu(u); (AtAu(v), v) }
    val fold (vBv, vv) = (0., 0.)
        for ui <- u, vi <- v { (vBv + ui*vi, vv + vi*vi) }
    Math.sqrt(vBv/vv)
}

val N = match Args.arguments() {
    | n_str :: [] => n_str.to_int_or(5500)
    | _ => 5500
    }
println(spectralnorm(N))
