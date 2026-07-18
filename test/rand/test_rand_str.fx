/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// T5 randomized suite: string slicing / find / split vs naive references,
// including Unicode (UTF-32) content.  Reproducible from a printed per-case seed.

from UTest import *
from RandUtil import *

// a random lowercase word of the given length (no separator chars)
fun rand_word(rng: rndstate_t, len: int): string =
    string([for k <- 0:len {chr(next_int(rng, ord('a'), ord('z')))}])

TEST("rand.str.slice", fun() {
    val name = "rand.str.slice"
    for i <- 0:400 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 1, 30)
        val s = rand_string(rng, n)
        val lo = next_int(rng, 0, n - 1), hi = next_int(rng, lo + 1, n)
        val got = s[lo:hi]
        val rf = string([for k <- lo:hi {s[k]}])
        if got != rf {println(f"  [repro] {name} case={i} seed={seed} n={n} lo={lo} hi={hi}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.str.find", fun() {
    val name = "rand.str.find"
    for i <- 0:400 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 4, 30)
        val s = rand_word(rng, n)                       // lowercase letters only
        val p = next_int(rng, 0, n - 2)
        val k = next_int(rng, 1, min(4, n - p))
        val needle = s[p:p + k]
        val fi = s.find(needle)
        // find returns the FIRST match; it must be a real match at <= p.
        val ok = fi >= 0 && fi <= p && s[fi:fi + k] == needle
        // an out-of-alphabet needle must be absent (-1)
        val absent = s.find("0" + needle)
        if !ok || absent != -1 {
            println(f"  [repro] {name} case={i} seed={seed} s='{s}' needle='{needle}' fi={fi}")
        }
        EXPECT(ok)
        EXPECT_EQ(absent, -1)
    }
})

TEST("rand.str.split_join", fun() {
    val name = "rand.str.split_join"
    for i <- 0:300 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val ntok = next_int(rng, 1, 8)
        val toks = [:: for t <- 0:ntok {rand_word(rng, next_int(rng, 1, 6))}]
        val joined = "|".join(toks)
        val parts = joined.split('|', allow_empty=true)
        var bad = parts.length() != toks.length()
        for p <- parts, t <- toks { if p != t {bad = true} }
        if bad {println(f"  [repro] {name} case={i} seed={seed} joined='{joined}'")}
        EXPECT(!bad)
    }
})

TEST("rand.str.concat", fun() {
    val name = "rand.str.concat"
    for i <- 0:400 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n1 = next_int(rng, 0, 20), n2 = next_int(rng, 0, 20)
        val s1 = rand_string(rng, n1), s2 = rand_string(rng, n2)
        val s = s1 + s2
        var bad = s.length() != n1 + n2
        if n1 > 0 && s[0:n1] != s1 {bad = true}
        if n2 > 0 && s[n1:n1 + n2] != s2 {bad = true}
        if bad {println(f"  [repro] {name} case={i} seed={seed} n1={n1} n2={n2}")}
        EXPECT(!bad)
    }
})

TEST("rand.str.unicode_length", fun() {
    val name = "rand.str.unicode_length"
    for i <- 0:400 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 0, 40)
        val s = rand_string(rng, n)                     // includes non-ASCII chars
        // length() counts characters (UTF-32), not bytes
        if s.length() != n {println(f"  [repro] {name} case={i} seed={seed} n={n} len={s.length()}")}
        EXPECT_EQ(s.length(), n)
    }
})
