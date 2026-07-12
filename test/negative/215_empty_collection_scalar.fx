// expect: does not match
// resolve-2: [] is typed TypVarCollection ("some list/rrbvec/array"), so
// assigning it to a scalar is now a TYPE CHECKER error. Before, [] was a
// fully free type var, `val n: int = []` passed -no-c, and the misuse was
// only caught at K-normalization ("[] is misused...") -- or, worse, the free
// var made bogus overload candidates viable (FB-007/S3).
val n: int = []
println(n)
