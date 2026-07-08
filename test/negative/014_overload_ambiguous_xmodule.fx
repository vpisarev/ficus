// expect: ambiguous call
// resolve-1: a local function re-declaring a stdlib helper with a
// byte-identical signature (Builtins.length(string), star-imported by the
// preamble) is EqGeneric with it -- neither can be more specific -- so a
// fully-determined call that sees both is a hard ambiguity error, and the
// candidate listing attributes each signature to its defining module/location
// (the cross-module case). The hint (qualify the call) is the resolution.
// NB: the original 014 case (local all-keywords-defaulted generic vs
// Math.sqrt(double)) stopped being ambiguous when keyword normalization
// landed in compare_fun_generality: the exact keywordless match now ranks
// more specific and wins.
fun length(s: string) = 42
val r = length("abc")
println(r)
