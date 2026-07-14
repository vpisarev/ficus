/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on vectors

operator .+ [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec = rrbvec(for ai <- a, bi <- b {ai + bi})
operator .+ [T](a: T rrbvec, b: T): T rrbvec = rrbvec(for ai <- a {ai + b})
operator .+ [T](a: T, b: T rrbvec): T rrbvec = rrbvec(for bi <- b {a + bi})
operator .- [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec = rrbvec(for ai <- a, bi <- b {ai - bi})
operator .- [T](a: T rrbvec, b: T): T rrbvec = rrbvec(for ai <- a {ai - b})
operator .- [T](a: T, b: T rrbvec): T rrbvec = rrbvec(for bi <- b {a - bi})
operator .* [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec = rrbvec(for ai <- a, bi <- b {ai * bi})
operator .* [T](a: T rrbvec, b: T): T rrbvec = rrbvec(for ai <- a {ai * b})
operator .* [T](a: T, b: T rrbvec): T rrbvec = rrbvec(for bi <- b {a * bi})
operator ./ [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec = rrbvec(for ai <- a, bi <- b {ai / bi})
operator ./ [T](a: T rrbvec, b: T): T rrbvec = rrbvec(for ai <- a {ai / b})
operator ./ [T](a: T, b: T rrbvec): T rrbvec = rrbvec(for bi <- b {a / bi})
operator .% [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec =
    rrbvec(for x <- a, y <- b {x .% y})
operator .** [T1, T2, T3](a: T1 rrbvec, b: T2 rrbvec): T3 rrbvec =
    rrbvec(for x <- a, y <- b {x .** y})
operator & [T](a: T rrbvec, b: T rrbvec): T rrbvec =
    rrbvec(for x <- a, y <- b {x & y})
operator | [T](a: T rrbvec, b: T rrbvec): T rrbvec =
    rrbvec(for x <- a, y <- b {x | y})
operator ^ [T](a: T rrbvec, b: T rrbvec): T rrbvec =
    rrbvec(for x <- a, y <- b {x ^ y})

operator .<=> [T](a: T rrbvec, b: T rrbvec): int rrbvec =
    rrbvec(for x <- a, y <- b {x <=> y})
operator .== [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {x == y})
operator .!= [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(x == y)})
operator .< [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {x < y})
operator .<= [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(y < x)})
operator .> [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {y < x})
operator .>= [T](a: T rrbvec, b: T rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(x < y)})
