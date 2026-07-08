// expect: ambiguous call
// resolve-1: a local generic whose keywords are all defaulted ties with the
// concrete Math.sqrt(double) from the preamble: per proposal §10.Q2 there is
// deliberately NO "fewer defaults preferred" tie-break, so the fully-determined
// call errors, and the candidate listing attributes each signature to its
// defining module/location (the cross-module case).
fun sqrt(a: 't, ~n: int=2) = a
val r = sqrt(81.0)
println(r)
