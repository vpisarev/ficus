/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// k-nucleotide benchmark from
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
// Converted from the Ruby version, authored by:
//    jose fco. gonzalez
//    Sokolov Yura
//    Rick Branson

import File, Sys
import Hashmap

type hashtab_t = (int64, int) Hashmap.t

fun encrypt_char(c: char): uint8
{
    | 'A' | 'a' => 0u8
    | 'C' | 'c' => 1u8
    | 'G' | 'g' => 2u8
    | _ => 3u8
}

fun decrypt_key(n: int64, len: int)
{
    val chars = [| for i <- 0:len {
        match (n >> (len-i-1)*2) & 3L {
            | 0L => 'A'
            | 1L => 'C'
            | 2L => 'G'
            | _ => 'T'
        } } |]
    string(chars)
}

fun frequency(seq: uint8 vector, len: int)
{
    val n = size(seq) + 1 - len
    val freq = Hashmap.empty(1<<14, 0L, 0)
    var key = 0L
    val mask = (1L << len*2) - 1
    for c@i <- seq {
        key = (key*4 + c) & mask
        if i < len-1 {continue}
        val idx = freq.find_idx_or_insert(key)
        freq.table[idx].data += 1
    }
    (n, freq)
}

fun sort_by_freq(seq: uint8 vector, length: int)
{
    val (total, freq) = frequency(seq, length)
    val sorted = freq.list().sort(fun ((_, n1), (_, n2)) {n1 > n2})
    "".join([for (wj, nj) <- sorted {f"{decrypt_key(wj, length)} {nj*100./total}\n"}])
}

fun find_seq(seq: uint8 vector, substr: string)
{
    val length = substr.length()
    val (_, freq) = frequency(seq, length)
    val fold key = 0L for c <- substr {key*4 + encrypt_char(c)}
    val idx = freq.find_idx(key)
    val freq = if idx >= 0 {freq.table[idx].data} else {0}
    f"{freq}\t{substr.toupper()}"
}

var lines: string list = []
val fname = match Sys.arguments() {
    | fname :: _ => fname
    | _ =>
        println("Missing file name. See the description: https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/knucleotide.html")
        throw Fail("missing file name")
    }
val f = File.open(fname, "rt")
while !f.eof() {
    val line = f.readln()
    if line.startswith('>') && line.startswith(">THREE") {
        break
    }
}

var all_lines: uint8 vector = []

while !f.eof() {
    val line = f.readln()
    if line.startswith('>') {break}
    val v = [<for c <- line.rstrip() { encrypt_char(c)}>]
    all_lines += v
}
f.close()

val report = [| @parallel for w <- [| "*", "**", "GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT" |] {
    if w.startswith("*") { sort_by_freq(all_lines, w.length()) }
    else {find_seq(all_lines, w)}
} |]

for l <- report {println(l)}
