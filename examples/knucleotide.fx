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
import Hashmap, Dynvec

type hashtab_t = (int64, int) Hashmap.t

val lut = {
    val L = array(256, 255u8)
    L[ord('A')] = 0
    L[ord('a')] = 0
    L[ord('C')] = 1
    L[ord('c')] = 1
    L[ord('G')] = 2
    L[ord('g')] = 2
    L[ord('T')] = 3
    L[ord('t')] = 3
    L
}

fun encrypt_char(c: char) = lut[int(c)]

fun decrypt_key(n: int64, len: int)
{
    val chars = [for i <- 0:len {
        match (n >> (len-i-1)*2) & 3i64 {
            | 0i64 => 'A'
            | 1i64 => 'C'
            | 2i64 => 'G'
            | _ => 'T'
        } }]
    string(chars)
}

fun frequency(seq: uint8 [], len: int)
{
    val n = size(seq)
    val freq = Hashmap.empty(32, 0i64, 0)
    var key = 0i64
    val mask = (1i64 << len*2) - 1
    for i <- 0:min(len-1, n) {
        key = (key*4 + seq[i]) & mask
    }

    for i <- len-1:n {
        key = (key*4 + seq[i]) & mask
        val idx = freq.find_idx_or_insert(key)
        freq.table[idx].data += 1
    }
    (n + 1 - len, freq)
}

fun sort_by_freq(seq: uint8 [], length: int)
{
    val (total, freq) = frequency(seq, length)
    val sorted = freq.list().sort(fun ((_, n1), (_, n2)) {n1 > n2})
    "".join([::for (wj, nj) <- sorted {f"{decrypt_key(wj, length)} {nj*100./total}\n"}])
}

fun find_seq(seq: uint8 [], substr: string)
{
    val length = substr.length()
    val (_, freq) = frequency(seq, length)
    val fold key = 0i64 for c <- substr {key*4 + encrypt_char(c)}
    val idx = freq.find_idx(key)
    val freq = if idx >= 0 {freq.table[idx].data} else {0}
    f"{freq}\t{substr.toupper()}"
}

var lines: string list = []
val fname = match Sys.arguments() {
    | fname :: _ => fname
    | _ =>
        println("reading DNA data from the standard input ...\n\
                 See https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/knucleotide.html")
        ""
    }
val f = if fname != "" {File.open(fname, "rt")} else {File.stdin}
while !f.eof() {
    val line = f.readln()
    if line.startswith('>') && line.startswith(">THREE") {
        break
    }
}

var all_data = Dynvec.create(0, 0u8)

while !f.eof() {
    val line = f.readln()
    if line.startswith('>') {break}
    val converted = [for c <- line.rstrip() {encrypt_char(c)}]
    all_data.push(converted)
}

val all_data = all_data.data[:all_data.count]

val report = [ @parallel for w <- [ "*", "**", "GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT" ] {
    if w.startswith("*") { sort_by_freq(all_data, w.length()) }
    else {find_seq(all_data, w)}
}]

for l <- report {println(l)}
