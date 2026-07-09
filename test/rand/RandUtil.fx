/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deterministic RNG utilities for the fxtest randomized suites (T5).
//
// Every random case is reproducible from a printed seed: the per-case seed is
// derived purely from a base seed (env FXTEST_SEED, default fixed constant),
// the test name and the case index, so a failure printed as "seed=..." can be
// replayed exactly.  Generator is splitmix64 (the OpenCV-proven pattern).

import Sys

// FB-002 (uint64 `>>` folded as an arithmetic shift) is fixed, so plain `>>`
// is used directly here; the splitmix64 reference vectors in the rand suites
// guard the fix forever.
fun splitmix64(x: uint64): uint64
{
    val z0 = x + 0x9e3779b97f4a7c15u64
    val z1 = (z0 ^ (z0 >> 30)) * 0xbf58476d1ce4e5b9u64
    val z2 = (z1 ^ (z1 >> 27)) * 0x94d049bb133111ebu64
    z2 ^ (z2 >> 31)
}

type rndstate_t = { s: uint64 ref }

fun mk_rng(seed: uint64): RandUtil.rndstate_t = rndstate_t { s = ref(seed) }

// splitmix64 stream: advance the state by the golden-ratio increment, then mix.
fun next_u64(rng: rndstate_t): uint64
{
    val ns = *rng.s + 0x9e3779b97f4a7c15u64
    *rng.s = ns
    val z1 = (ns ^ (ns >> 30)) * 0xbf58476d1ce4e5b9u64
    val z2 = (z1 ^ (z1 >> 27)) * 0x94d049bb133111ebu64
    z2 ^ (z2 >> 31)
}

fun next_i32(rng: rndstate_t): int32 = ((next_u64(rng) :> uint32) :> int32)
fun next_u8(rng: rndstate_t): uint8 = (next_u64(rng) :> uint8)

// uniform integer in [a, b] (inclusive)
fun next_int(rng: rndstate_t, a: int, b: int): int
{
    val (a, b) = (min(a, b), max(a, b))
    val range = (b - a + 1 :> uint64)
    ((next_u64(rng) % range) :> int) + a
}

fun next_double(rng: rndstate_t): double =
    next_u64(rng) * 5.42101086242752217003726400434970855712890625e-20

fun next_double(rng: rndstate_t, a: double, b: double): double =
    next_double(rng) * (b - a) + a

// --- random containers -------------------------------------------------------
fun rand_iarray(rng: rndstate_t, n: int, a: int, b: int): int [] =
    [for i <- 0:n {next_int(rng, a, b)}]

fun rand_darray(rng: rndstate_t, n: int, a: double, b: double): double [] =
    [for i <- 0:n {next_double(rng, a, b)}]

fun rand_ilist(rng: rndstate_t, n: int, a: int, b: int): int list =
    [:: for i <- 0:n {next_int(rng, a, b)}]

// printable ASCII, with a sprinkling of Latin/Greek to exercise UTF-32 strings.
fun rand_string(rng: rndstate_t, n: int): string =
    string([for i <- 0:n {
        val cp = if next_int(rng, 0, 99) < 80 {next_int(rng, 32, 126)}
                 else {next_int(rng, 0xa1, 0x24f)}
        chr(cp)
    }])

// --- reproducible per-case seeding ------------------------------------------
val g_default_base_seed = 0x1234567890abcdefu64

fun base_seed(): uint64
{
    val e = Sys.getenv("FXTEST_SEED", "")
    match e.to_int() {
    | Some(n) => (n :> uint64)
    | _ => g_default_base_seed
    }
}

fun str_hash(s: string): uint64
{
    var h = 0xcbf29ce484222325u64        // FNV-1a 64-bit
    for c <- s {
        h = (h ^ (ord(c) :> uint64)) * 0x100000001b3u64
    }
    h
}

fun case_seed(name: string, idx: int): uint64 =
    splitmix64(base_seed() ^ str_hash(name) ^ (idx :> uint64))

fun mk_case_rng(name: string, idx: int): RandUtil.rndstate_t = mk_rng(case_seed(name, idx))
