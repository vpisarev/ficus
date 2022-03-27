/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on vectors

operator .+ (a: 't1 vector, b: 't2 vector) = vector [for ai <- a, bi <- b {ai + bi}]
operator .+ (a: 't vector, b: 't) = vector [for ai <- a {ai + b}]
operator .+ (a: 't, b: 't vector) = vector [for bi <- b {a + bi}]
operator .- (a: 't1 vector, b: 't2 vector) = vector [for ai <- a, bi <- b {ai - bi}]
operator .- (a: 't vector, b: 't) = vector [for ai <- a {ai - b}]
operator .- (a: 't, b: 't vector) = vector [for bi <- b {a - bi}]
operator .* (a: 't1 vector, b: 't2 vector) = vector [for ai <- a, bi <- b {ai * bi}]
operator .* (a: 't vector, b: 't) = vector [for ai <- a {ai * b}]
operator .* (a: 't, b: 't vector) = vector [for bi <- b {a * bi}]
operator ./ (a: 't1 vector, b: 't2 vector) = vector [for ai <- a, bi <- b {ai / bi}]
operator ./ (a: 't vector, b: 't) = vector [for ai <- a {ai / b}]
operator ./ (a: 't, b: 't vector) = vector [for bi <- b {a / bi}]
operator .% (a: 'ta vector, b: 'tb vector) =
    vector [for x <- a, y <- b {x .% y}]
operator .** (a: 'ta vector, b: 'tb vector) =
    vector [for x <- a, y <- b {x .** y}]
operator & (a: 't vector, b: 't vector) =
    vector [for x <- a, y <- b {x & y}]
operator | (a: 't vector, b: 't vector) =
    vector [for x <- a, y <- b {x | y}]
operator ^ (a: 't vector, b: 't vector) =
    vector [for x <- a, y <- b {x ^ y}]

operator .<=> (a: 't vector, b: 't vector): int vector =
    vector [for x <- a, y <- b {x <=> y}]
operator .== (a: 't vector, b: 't vector): bool vector =
    vector [for x <- a, y <- b {x == y}]
operator .!= (a: 't vector, b: 't vector): bool vector =
    vector [for x <- a, y <- b {!(x == y)}]
operator .< (a: 't vector, b: 't vector) =
    vector [for x <- a, y <- b {x < y}]
operator .<= (a: 't vector, b: 't vector): bool vector =
    vector [for x <- a, y <- b {!(y < x)}]
operator .> (a: 't vector, b: 't vector): bool vector =
    vector [for x <- a, y <- b {y < x}]
operator .>= (a: 't vector, b: 't vector): bool vector =
    vector [for x <- a, y <- b {!(x < y)}]
