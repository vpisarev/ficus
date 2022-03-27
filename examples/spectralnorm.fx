/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Sys, Math

fun A(i: int, j: int) =
    double(((i + j) * (i + j + 1) >> 1) + i + 1)

fun Au(u: double[], res: double [])
{
    val N = size(u)

    @parallel for i <- 0:N {
        res[i] = fold t = 0. for uj@j <- u {
            t + uj / A(i, j)
        }
    }
}

fun Atu(u: double[], res: double [])
{
    val N = size(u)

    @parallel for i <- 0:N {
        res[i] = fold t = 0. for uj@j <- u {
            t + uj / A(j, i)
        }
    }
}

fun AtAu(u: double[], res: double [], temp: double []) {
    Au(u, temp)
    Atu(temp, res)
}

fun spectralnorm(n: int)
{
    val temp = array(n, 0.)
    val u = array(n, 1.), v = array(n, 0.)
    for i <- 0:10 { AtAu(u, v, temp); AtAu(v, u, temp) }
    val fold vBv=0., vv = 0.
        for ui <- u, vi <- v { (vBv + ui*vi, vv + vi*vi) }
    sqrt(vBv/vv)
}

val N = match Sys.arguments() {
    | n_str :. => n_str.to_int_or(5500)
    | _ => 5500
    }
println(spectralnorm(N))
