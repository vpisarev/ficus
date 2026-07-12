/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on vectors

operator .+ (a: 't1 rrbvec, b: 't2 rrbvec): 't3 rrbvec = rrbvec(for ai <- a, bi <- b {ai + bi})
operator .+ (a: 't rrbvec, b: 't): 't rrbvec = rrbvec(for ai <- a {ai + b})
operator .+ (a: 't, b: 't rrbvec): 't rrbvec = rrbvec(for bi <- b {a + bi})
operator .- (a: 't1 rrbvec, b: 't2 rrbvec): 't3 rrbvec = rrbvec(for ai <- a, bi <- b {ai - bi})
operator .- (a: 't rrbvec, b: 't): 't rrbvec = rrbvec(for ai <- a {ai - b})
operator .- (a: 't, b: 't rrbvec): 't rrbvec = rrbvec(for bi <- b {a - bi})
operator .* (a: 't1 rrbvec, b: 't2 rrbvec): 't3 rrbvec = rrbvec(for ai <- a, bi <- b {ai * bi})
operator .* (a: 't rrbvec, b: 't): 't rrbvec = rrbvec(for ai <- a {ai * b})
operator .* (a: 't, b: 't rrbvec): 't rrbvec = rrbvec(for bi <- b {a * bi})
operator ./ (a: 't1 rrbvec, b: 't2 rrbvec): 't3 rrbvec = rrbvec(for ai <- a, bi <- b {ai / bi})
operator ./ (a: 't rrbvec, b: 't): 't rrbvec = rrbvec(for ai <- a {ai / b})
operator ./ (a: 't, b: 't rrbvec): 't rrbvec = rrbvec(for bi <- b {a / bi})
operator .% (a: 'ta rrbvec, b: 'tb rrbvec): 't3 rrbvec =
    rrbvec(for x <- a, y <- b {x .% y})
operator .** (a: 'ta rrbvec, b: 'tb rrbvec): 't3 rrbvec =
    rrbvec(for x <- a, y <- b {x .** y})
operator & (a: 't rrbvec, b: 't rrbvec): 't rrbvec =
    rrbvec(for x <- a, y <- b {x & y})
operator | (a: 't rrbvec, b: 't rrbvec): 't rrbvec =
    rrbvec(for x <- a, y <- b {x | y})
operator ^ (a: 't rrbvec, b: 't rrbvec): 't rrbvec =
    rrbvec(for x <- a, y <- b {x ^ y})

operator .<=> (a: 't rrbvec, b: 't rrbvec): int rrbvec =
    rrbvec(for x <- a, y <- b {x <=> y})
operator .== (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {x == y})
operator .!= (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(x == y)})
operator .< (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {x < y})
operator .<= (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(y < x)})
operator .> (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {y < x})
operator .>= (a: 't rrbvec, b: 't rrbvec): bool rrbvec =
    rrbvec(for x <- a, y <- b {!(x < y)})
