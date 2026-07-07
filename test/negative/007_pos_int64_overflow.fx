// expect: out of range
// FB-014: bare positive 2^63 has no int64 representation (only -2^63 does).
val a = 9223372036854775808
println(a)
