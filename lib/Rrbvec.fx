/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on vectors

operator .+ [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] = rrbvec(for ai <- a, bi <- b {ai + bi})
operator .+ [T](a: rrbvec[T], b: T): rrbvec[T] = rrbvec(for ai <- a {ai + b})
operator .+ [T](a: T, b: rrbvec[T]): rrbvec[T] = rrbvec(for bi <- b {a + bi})
operator .- [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] = rrbvec(for ai <- a, bi <- b {ai - bi})
operator .- [T](a: rrbvec[T], b: T): rrbvec[T] = rrbvec(for ai <- a {ai - b})
operator .- [T](a: T, b: rrbvec[T]): rrbvec[T] = rrbvec(for bi <- b {a - bi})
operator .* [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] = rrbvec(for ai <- a, bi <- b {ai * bi})
operator .* [T](a: rrbvec[T], b: T): rrbvec[T] = rrbvec(for ai <- a {ai * b})
operator .* [T](a: T, b: rrbvec[T]): rrbvec[T] = rrbvec(for bi <- b {a * bi})
operator ./ [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] = rrbvec(for ai <- a, bi <- b {ai / bi})
operator ./ [T](a: rrbvec[T], b: T): rrbvec[T] = rrbvec(for ai <- a {ai / b})
operator ./ [T](a: T, b: rrbvec[T]): rrbvec[T] = rrbvec(for bi <- b {a / bi})
operator .% [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] =
    rrbvec(for x <- a, y <- b {x .% y})
operator .** [T1, T2, T3](a: rrbvec[T1], b: rrbvec[T2]): rrbvec[T3] =
    rrbvec(for x <- a, y <- b {x .** y})
operator & [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[T] =
    rrbvec(for x <- a, y <- b {x & y})
operator | [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[T] =
    rrbvec(for x <- a, y <- b {x | y})
operator ^ [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[T] =
    rrbvec(for x <- a, y <- b {x ^ y})

operator .<=> [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[int] =
    rrbvec(for x <- a, y <- b {x <=> y})
operator .== [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {x == y})
operator .!= [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {!(x == y)})
operator .< [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {x < y})
operator .<= [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {!(y < x)})
operator .> [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {y < x})
operator .>= [T](a: rrbvec[T], b: rrbvec[T]): rrbvec[bool] =
    rrbvec(for x <- a, y <- b {!(x < y)})
