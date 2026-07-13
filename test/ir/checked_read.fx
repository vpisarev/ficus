// IR snapshot: FB-027 -- purity vs effect-freedom in dead-code elimination.
// A border read (.clip) never throws, so a dead one is eliminated; a checked
// (BorderNone) read can raise OutOfRangeError, so a dead one is retained (its
// bounds check is a side effect).  See docs/found_bugs.md FB-027.
fun probe(a: int []): int
{
    ignore(a.clip[100])   // never throws  -> removed by DCE
    ignore(a[7])          // BorderNone    -> retained (checked read may throw)
    a[0]
}
println(probe([1, 2, 3, 4, 5]))
