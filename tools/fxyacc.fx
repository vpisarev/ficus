/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
A simple generator of LALR(1) parsers in ficus.
This is a port of miniyacc (https://c9x.me/yacc/).
Below is the original license:

=====
MIT/X Consortium License

© 2015 Quentin Carbonneaux

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
=====
*/

import File, Set, Sys

type sym_t = Sym: int
type rule_t = { lhs: Sym; rhs: Sym list; act: string; actln: int; prec: int }
type tset_t = { bitmap: uint32 [] }

type assoc_t = ANone | ALeft | ARight | ANonassoc
type info_t = { nul: bool; fst: tset_t; prec: int; assoc: assoc_t; name: string; typ: string }
type term_t = { rule: int; dot: int; lk: tset_t }
type item_t = { idx: int; ts: term_t []; gtbl: int []; dirty: bool ref}
type row_t = { defval: int; ndef: int; tab: int []}

val TkEOF = Sym(0)
val MaxNt = 500
val MaxTk = 500
val StartNT = MaxTk

val g_srs = "shift/reduce conflict state %d token %s\n"
val g_rrs = "reduce/reduce conflict state %d token %s\n"

type yacc_options_t =
{
    vf: bool;
    df: bool;
    prefix: string;
    grammar: string;
}

type yacc_state_t =
{
    rs: rule_t []; // grammar rules (ordered, rcmp)
    rs_start: int []; // start rule for non-terminals
    is: info_t []; // symbol information
    nsy: int; // number of grammar symbols (terminals + non-terminals)
    nst: int; // number of states
    ntk: int; // number of tokens
    st: item_t Set.set_t; // LALR(1) states (ordered, icmp)
    atbl: row_t []; // action table [state][tok]
    gtbl: row_t []; // goto table   [sym][state]
    start: sym_t; // start symbol
    ini: int; // initial state
    doty: bool; // type-checking enabled
    zero_tset: tset_t;

    srconflicts: int;
    rrconflicts: int;

    act: int [];
    chk: int [];
    adsp: int [];
    gdsp: int [];

    lineno: int;
    inp: string;
    inpos: int;

    fout: File.t;
    fgrm: File.t;
    fhdr: File.t;

    grammar_strm: byte []
    grammar_pos: int;
    grammar_lineno: int;

    opts: yacc_options_t;
}

fun init_yacc() = yacc_state_t
{
    rs = (array() : rule_t []),
    rs_start = (array() : int []),
    is = (array() : info_t []),
    nsy = 0,
    nst = 0,
    ntk = 0,
    st = Set.empty(icmp),
    atbl = (array() : row_t []),
    gtbl = (array() : row_t []),
    start = Sym(-1),
    ini = 0,
    doty = false,
    zero_tset = {bitmap=(array(): uint32 [])},

    srconflicts = 0,
    rrconflicts = 0,

    act = (array(): int []),
    chk = (array(): int []),
    adsp = (array(): int []),
    gdsp = (array(): int []),

    lineno = 1,
    inp = "",
    inpos = 0,

    outp = ([] : string list),
    outgrm = ([] : string list),
    outhdr = ([] : string list)
}

var g_yacc = init_yacc()

fun Red(i: int) = -(i+2)

exception YaccExit: int
fun die(s: string) = throw Fail(f"{s} (on line {g_yacc.lineno})")

operator == (Sym(s1): sym_t, Sym(s2): sym_t) = s1 == s2
operator <=> (Sym(s1): sym_t, Sym(s2): sym_t) = s1 <=> s2
fun symname(Sym(s): sym_t) = g_yacc.is[s].name

fun rcmp(a: rule_t, b: rule_t) = a.lhs - b.lhs

operator == (a: assoc_t, b: assoc_t)
{
    | (ANone, ANone) | (ALeft, ALeft) | (ARight, ARight) | (ANonassoc, ANonassoc) => true
    | _ => false
}

fun copy(t: term_t) = term_t { rule=t.rule, dot=t.dot, lk=copy(t.lk) }

fun find_rules(Sym(lhs): sym_t)
{
    val StartNT = g_yacc.ntk
    if lhs < StartNT || lhs >= g_yacc.nsy {(-1, -1)}
    else {(g_yacc.rs_start[lhs - StartNT], g_yacc.rs_start[lhs - StartNT + 1])}
}

fun tset_size() = (g_yacc.ntk + 31)/32
fun get_rule(r: int) = g_yacc.rs[r]

fun ts_zero() = tset_t { bitmap=[for i<-0:tset_size() {0u32}] }
fun ts_union(a: tset_t, b: tset_t) =
    fold changed=false for i <- 0:tset_size() {
        val tmp = a[i] | b[i]
        val changed = changed | (tmp != a[i])
        a[i] = tmp
        changed
    }

fun ts_setbit(a: tset_t, n: int) = a.bitmap[n/32] |= uint32(1 << (n % 32))
fun ts_getbit(a: tset_t, n: int) = (a.bitmap[n/32] & uint32(1 << (n % 32))) != 0u32

// computes FIRST(rhs), i.e. the set of terminals that can start expanded stnc.
fun first(stnc: Sym list, last: tset_t)
{
    fun first_(ts: tset_t, stnc: Sym list) =
    match stnc {
    | Sym(f) :: rest =>
        if f < g_yacc.ntk { ts_setbit(ts, f) }
        else {
            tsunion(ts, is[f].fst)
            if g_yacc.is[f].nul { first_(ts, rest) }
        }
    | _ | Sym(0) :: _ => tsunion(ts, last)
    }
    val ts = ts_zero()
    first_(ts, stnc)
    ts
}

fun ginit()
{
    var changed = false
    for iters <- 0:100000 {
        changed = false
        for r <- g_yacc.rs {
            val Sym(lhs) = r.lhs
            val have_nonul = exists(for s <- r.rhs {
                val Sym(s_idx) = s
                //if s_idx == 0 { break with false }
                !g_yacc.is[s_idx].nul
            })
            if !have_nonul {
                changed |= !g_yacc.is[lhs].nul
                g_yacc.is[lhs].nul = true
            }
            val ts = first(r->rhs, g_yacc.zero_tset)
            changed |= tsunion(g_yacc.is[lhs].fst, ts)
        }
        if !changed {break}
    }
    if changed {die("too much iterations to initialize the state")}
}

fun tcmp(a: term_t, b: term_t) = if a.rule != b.rule { a.rule - b.rule } else { a.dot - b.dot }
fun tcmp_lt(a: term_t, b: term_t) = tcmp(a, b) < 0

// computes CLOSURE(i0) for state/item i0 and returns the updated copy of i0
fun iclose(i0: item_t)
{
    var i = i0
    val smap = [for i<-0:MaxNt {0}]
    val StartNT = g_yacc.ntk

    for t@n <- i.ts {
        if t.dot == 0 {
            val Sym(lhs) = get_rule(t.rule).lhs
            val s = lhs - StartNT
            if smap[s] == 0 { smap[s] = n }
        }
    }

    var changed = true
    for iter <- 0:100000 {
        changed = false
        val ts_curr = i.ts
        for t <- ts_curr {
            val rem = g_yacc.rs[t.rule].rhs.skip(t.dot)
            match rem {
            | Sym(s) :: rem =>
                if s >= StartNT {
                    val (r0, r1) = find_rules(Sym(s))
                    if (r0 < 0) { die(f"the non-terminal {s} is not defined") }
                    val lk = first(rem, t.lk)
                    val m = smap[s-StartNT]
                    if m > 0 {
                        ignore(fold tt = i.ts.skip_nothrow(m) for r <- r0:r1 {
                            if tt.null() {break with tt}
                            changed |= ts_union(tt.hd().lk, lk)
                            tt.tl()
                        })
                    }
                    else {
                        val m = i.ts.length()
                        smap[s - StartNT] = m
                        var new_ts = []
                        for r <- r0:r1 {
                            if get_rule(r).lhs != Sym(s) {break}
                            new_ts = (term_t {rule=r, dot=0, lk=copy(lk)}) :: new_ts
                        }
                        i.ts += new_ts.rev()
                        changed = true
                    }
                }
            | _ => {}
            }
        }
        if !changed {break}
    }
    if changed {die("too much iterations to compute the closure of the item")}
    i.ts = i.ts.sort(tcmp_lt)
    i
}

// computes GOTO(i, s) for state/item i and symbol s
fun igoto(i: item_t, s: sym_t)
{
    val fold new_ts = ([]: term_t list) for t <- i.ts {
        val rhs = get_rule(t.rule).rhs
        if rhs.length() <= t.dot || rhs.nth(t.dot) != s { new_ts }
        else { t.{dot = t.dot+1} :: new_ts }
    }

    new_ts.rev().sort(tcmp_lt)
}

// compares two items using their sorted lists of terms
fun icmp(a: item_t, b: item_t)
{
    fun icmp_(ats: term_t list, bts: term_t list) {
        | (ai :: arest, bi :: brest) =>
            val d = int(ai.dot != 0) - int(bi.dot != 0)
            if d != 0 {d}
            else {
                val c = tcmp(ai, bi)
                if c != 0 {c} else { icmp_(arest, brest) }
            }
        | (ai :: arest, []) => if ai.dot != 0 {1} else {0}
        | ([], bi :: brest) => if bi.dot != 0 {-1} else {0}
        | _ => 0
    }
    icmp_(a.ts, b.ts)
}

// adds another LALR(1) state if it's not there yet
fun stadd(i0: item_t): (item_t, bool) =
    match g_yacc.st.find_opt(i0) {
    | Some(i) =>
        val fold changed=false for t0 <- i0.ts, t1 <- i.ts {
            changed | ts_union(t.lk, t0.lk)
        }
        *i.dirty |= changed
        (i, changed)
    | _ =>
        g_yacc.st = g_yacc.st.add(i0)
        (i0, changed)
    }

fun new_item(ts: term_t list) =
    item_t {
        idx = g_yacc.st.size+1,
        ts=ts,
        gtbl=array(g_yacc.nsy, 0),
        dirty=ref(true)
    }

// generates LALR(1) states
fun stgen()
{
    val StartNT = g_yacc.ntk
    val tini = term_t {rule=find_rules(Sym(StartNT)).0, dot=0, lk=ts_zero()}
    ts_setbit(tini.lk, 0)
    ignore(stadd(new_item(tini :: [])))

    var changed = true
    for iter <- 0:100000 {
        changed = false
        g_yacc.st.app(fun i ->
            if *i.dirty {
                *i.dirty = false
                val i = iclose(i)
                g_yacc.st = g_yacc.st.add()
                for s <- 0:g_yacc.nsy {
                    val ts = igoto(i, Sym(s))
                    val j = new_item(ts)
                    if !j.ts.null() {
                        i.gtbl[s] = 0
                    } else {
                        val (j, ch) = stadd(j)
                        changed |= ch
                        i.gtbl[s] = j.idx
                    }
                }
            })
        if(!changed) {break}
    }
    if changed {die("too much iterations to build the set of states")}
}

// resolve shift/reduce conflicts based on the operation priority and associativity
fun resolve(r: rule_t, Sym(s): sym_t, st: int)
{
    fun sr_conflict(s: int; st: int) {
        g_yacc.outgrm = f"shift/reduce conflict state {st} token {symname(Sym(s))}\n" :: g_yacc.outgrm
        g_yacc.srconflicts += 1
        ARight
    }

    val is = g_yacc.is[s]
    if r.prec == 0 || is.prec == 0 {
        sr_conflict(s, st)
    } else if r.prec == is.prec {
        if is.assoc == ANone {sr_conflict(s, st)}
        else is.assoc
    } else if r.prec < is.prec {
        ARight
    } else {
        ALeft
    }
}

// update the table of actions: positive values mean shift, negative values mean reduction
fun tblset(tbl: int [], i: item_t, t: term_t, renum_tab: int [])
{
    fun rr_conflict(s: int; st: int) {
        g_yacc.outgrm = f"reduce/reduce conflict state {st} token {symname(Sym(s))}\n" :: g_yacc.outgrm
        g_yacc.srconflicts += 1
        ARight
    }

    val rem = get_rule(t.rule).rhs.skip(t.dot)
    match rem {
    | Sym(s) :: rem =>
        // shift
        if (s < g_yacc.ntk) {
            val idx = renum_tab[i.gtbl[s]]
            val tbl_s = tbl[s]
            if idx < 0 {die(f"goto index for item {i.idx} and symbol {symname(Sym(s))} is negative")}
            val act =
                if (tbl_s != 0 && tbl_s != idx) {
                    if tbl_s > 0 {die(f"shift {tbl_s}/shift {idx} conflict?!")}
                    resolve(g_yacc.rs[Red(tbl_s)], s, i.idx-1)
                } else {ARight}
            match act {
            | ARight => tbl[s] = idx
            | ANonassoc => tbl[s] = -1
            | _ => {}
            }
        }
    } else {
        // reduce
        for s <- 0:g_yacc.ntk {
            if (!ts_getbit(t.lk, s)) { continue }
            // default to shift if conflict occurs
            val act = if tbl[s] == 0 { ALeft }
                else if tbl[s] < 0 { rr_conflict(s, i.idx-1) }
                else { resolve(get_rule(t.rule), s, i.idx-1) }

            match act {
            | ALeft => tbl[s] = Red(t.rule-rs)
            | ANonassoc => tbl[s] = -1
            | _ => {}
            }
        }
    }
}

fun setdef(tab: int [], top: int)
{
    val w = size(r.tab)
    val fold (ndef, maxval, defval) = (0, 0, -1) for x@n <- tab {
        if (x != 0 && x < top) {
            val fold cnt = 1 for m <- n+1:w { cnt + int(tab[m] == x) }
            if cnt > maxval {
                (ndef, cnt, x)
            } else {
                (ndef, maxval, defval)
            }
        } else {
            (ndef + int(x==0), maxval, defval)
        }
    }

    val ndef = if maxval == 0 {ndef}
    else
        fold ndef=ndef for x@n <- tab {
            if x == defval {
                // zero out the most frequent entry
                tab[n] = 0
                ndef + 1
            } else { ndef }
        }

    row_t { defval=defval, ndef=ndef, tab=tab }
}

fun tblgen()
{
    val all_states = g_yacc.st.list()
    val nst = g_yacc.st.size
    g_yacc.nst = nst

    // renumerte all the states
    val renum_tab = array(nst+1, 0)
    renum_tab[0] = 0
    for i@n <- all_states {
        renum_tab[i.idx] = n+1
    }

    // fill action table
    g_yacc.atbl = [for i@n <- all_states {
        val tab = array(g_yacc.ntk, 0)
        for t <- i.ts {
            tblset(tab, i, t, renum_tab)
        }
        val r = setdef(tab, -1)
        r.{defval = Red(r.defval)} // Red(-1) == -1
    }]

    // fill goto table
    g_yacc.gtbl = [for n <- g_yacc.ntk:g_yacc.nsy {
        val tab = [for i <- all_states {
            val idx = i.gtbl[n]
            if idx != 0 {renum_tab[idx]} else {0}
        }]
        setdef(tab, nst+1)
    }]
}

fun prcmp(a: row_t, b: row_t) = a.ndef - b.ndef

fun actgen()
{
    val ntk = g_yacc.ntk
    val nsy = g_yacc.nsy
    val nst = g_yacc.nst

    var actsz = 0
    val act = array(nst*nsy, 0)
    val chk = array(nst*nsy, -1)
    val adsp = array(nst, 0)

    // fill in actions
    val aidx = [for idx <- 0:nst {idx}]
    sort(aidx, fun(j: int, k: int) {g_yacc.atbl[j].ndef - g_yacc.atbl[k].ndef})
    for idx <- aidx {
        val r_tab = g_yacc.atbl[idx].tab
        var dsp = 0
        for rm <- r_tab {
            if rm != 0 {break}
            dsp -= 1
        }
        var retrya = true
        while retrya {
            retrya = false
            for t <- 0:ntk {
                // The invariant here is even
                // trickier than it looks.
                val m = dsp + t
                if m >= 0 && chk[m] >= 0 &&
                   ((r_tab[t] != 0 && (chk[m] != t || act[m] != r_tab[t])) ||
                    (r_tab[t] == 0 && chk[m] == t)) {
                    dst += 1
                    retrya = true
                    break
                }
            }
        }
        adsp[idx] = dsp;
        for t <- 0:ntk {
            if r_tab[t] != 0 {
                chk[dsp+t] = t
                act[dsp+t] = r_tab[t]
                if dsp+t>=actsz {
                    actsz = dsp+t+1
                }
            }
        }
    }

    // fill in gotos
    val nnt = nsy - ntk
    val gdsp = array(nnt, 0)
    val gidx = [for idx <- 0:nnt {idx}]
    sort(gidx, fun(j: int, k: int) {g_yacc.gtbl[j].ndef - g_yacc.gtbl[k].ndef})

    for idx <- gidx {
        val r_tab = g_yacc.gtbk[idx]
        var dsp = 0, m = 0
        for rm <- r_tab {
            if rm != 0 {break}
            dst -= 1
            m += 1
        }
        var retryg = true
        while retryg {
            retryg = false
            for t <- m:nst {
                if chk[dsp+t]>=0 && r_tab[t] != 0 {
                    dsp += 1
                    retryg = true
                    break
                }
            }
            gdsp[idx] = dsp;
            for t <- m:nst {
                if r_tab[t] != 0 {
                    chk[dsp+t] = ntk + idx
                act[dsp+t] = r_tab[t]
                if dsp+t>=actsz {
                    actsz = dsp+t+1
                }
            }
        }
    }
    (act[:actsz], chk[:actsz], adsp, gdsp)
}

fun string_wz(i: int, ~width=0: int, ~zeropad: bool=false)
{
    pure fun string_wz_(i: int, w: int, z: bool) = ccode
    {
        char buf[128];
        if (w != 0) w = w < 126 ? w : 126;
        if (w == 0)
            sprintf(buf, "%d", (int)i);
        else if (!z)
            sprintf(buf, "%*d", (int)w, (int)i);
        else
            sprintf(buf, "%*0d", (int)w, (int)i);
        return fx_ascii2str(buf, -1, fx_result);
    }
    string_wz(i, width, zeropad)
}

fun aout(fout: File.t, name: string, tbl: int [])
{
    fout.print(f"short {name}[] = \{")
    for x@i <- tbl {
        if i % 10 == 0 {fout.println()}
        val s = string_wz(x, width=4)
        fout.print(s)
        if i != n-1 {fout.print(", ")}
    }
    fout.print("\n};\n")
}

fun tblout(fout: File.t, fhdr: File.t)
{
    fout.print("short yyini = {g_yacc.ini.idx-1};\n");
    fout.print("short yyntoks = {g_yacc.ntk};\n");

    val o = [for r <- g_yacc.rs {r.rhs.length()}]
    aout(fout, "yyr1", o)

    val o = [for {lhs=Sym(lhs)} <- g_yacc.rs {lhs - g_yacc.ntk}]
    aout(fout, "yyr2", o)

    val o = [for a <- g_yacc.atbl {a.defval}]
    aout(fout, "yyadef", o)

    val o = [for a <- g_yacc.gtbl {
        val defval = a.defval
        if defval > 0 {defval-1} else {defval}
    }]
    aout(fout, "yygdef", o)

    val (act, chk, adsp, gdsp) = actgen()

    aout(fout, "yyadsp", adsp)
    aout(fout, "yygdsp", gdsp)

    val act = [for a <- act {if a >= 0 {a-1} else {a}}]
    aout(fout, "yyact", act)
    aout(fout, "yychk", chk)

    val o1 = [for n <- 0:128 {
        if n < 32 {0} else {
        fold c=0 for m <- 0:g_yacc.ntk {
            if symname(Sym(m)).startswith(f"'{chr(n)}") {break with m}
        }}
    }]

    var m = 128
    val o2 = [: for n <- 1:g_yacc.ntk {
        val name = symname(Sym(n))
        if name.startswith("'") {continue}
        fout.println(f"#define {name} {m}")
        if fhdr.is_open() {fhdr.println(f"#define {name} {m}")}
        m += 1
        n
    } :]
    aout(fout, "yytrns", [\o1, \o2])
    if fhdr.is_open() {
        fhdr.println("int yyparse(void);
#ifndef YYSTYPE
#define YYSTYPE int
#endif
extern YYSTYPE yylval;
#endif")
    }
}

fun stdump(fgrm: File.t)
{
    if !fgrm.is_open() {
        throw NullFileError
    }

    for r@n <- g_yacc.rs {
        val name = symname(r.lhs)
        fgrm.println(f"\n{string_wz(n, width=3, zeropad=true)}: {name} ->")
        for s <- r.rhs {
            if s == Sym(0) {break}
            fgrm.print(f" {symname(s)}")
        }
    }
    fgrm.println()
    val all_states = g_yacc.st.list()
    for i@m <- all_states {
        fgrm.println(f"\nState {m}\n")
        for t@t_idx <- i.ts {
            val r = get_rule(t.rule)
            val d = t.dot;
            if d == 0 && t_idx != 0 {continue}
            fgrm.print(f"  {symname(r.lhs)} ->")
            var k = 0
            for sk <- r.rhs {
                if sk == TkEOF {break}
                val dstr = if k == d {". "} else {""}
                fgrm.print(" {dstr}{symname(sk)}")
                k += 1
            }
            if d == k { fgrm.print(" .") }
            fgrm.println()
        }
        fgrm.println()
        for act@n <- g_yacc.atbl[m] {
            if (act == 0) {continue}
            val name = symname(Sym(n))
            if (act==-1) {
                fgrm.println(f"  {name} error (nonassoc)")
            } else if (act<0) {
                fgrm.println(f"  {name} reduce with rule {Red(act)}")
            } else {
                fgrm.println(f"  {name} shift and go to {act-1}")
            }
        }
        val defval = g_yacc.atbl[m].defval
         if (defval != -1)
            fgrm.println("  * reduce with rule {defval}")
    }
}

type yacc_token_t =
    | TIdnt : string // ident
    | TTokchr : char // 'c'
    | TPP // %%
    | TLL // %{
    | TLangle // <
    | TRangle // >
    | TSemi // ;
    | TBar // |
    | TColon // :
    | TLBrack // {
    | TUnion
    | TType
    | TToken
    | TRight
    | TLeft
    | TNonassoc
    | TPrec
    | TStart
    | TEof

operator == (a: yacc_token_t, b: yacc_token_t)
{
    | (TPP, TPP) | (TLL, TLL)
    | (TLangle, TLangle) | (TRangle, TRangle)
    | (TSemi, TSemi) | (TBar, TBar)
    | (TColon, TColon) | (TLBrack, TLBrack)
    | (TUnion, TUnion) | (TType, TType)
    | (TToken, TToken) | (TRight, TRight)
    | (TLeft, TLeft) | (TNonassoc, TNonassoc)
    | (TPrec, TPrec) | (TStart, TStart)
    | (TEof, TEof) => true
    | (TIdnt(s1), TIdnt(s2)) => s1 == s2
    | (TTokchr(c1), TTokchr(c2)) => c1 == c2
    | _ => false
}

val yacc_keywords =
[:
    ("%%", TPP),
    ("%union", TUnion),
    ("%type", TType),
    ("%token", TToken),
    ("%right", TRight),
    ("%left", TLeft),
    ("%nonassoc", TNonassoc),
    ("%prec", TPrec),
    ("%start", TStart)
:]

fun istok(c: char) = c.isalnum() || c == '_' || c == '%'

fun nextchar()
{
    if c =
}

fun nexttk()
{
    int n;
    char c, *p;

    while isspace(c=fgetc(fin)))
        if (c == '\n')
            lineno++;
    switch (c) {
    case '<':
        return TLangle;
    case '>':
        return TRangle;
    case ';':
        return TSemi;
    case '|':
        return TBar;
    case ':':
        return TColon;
    case '{':
        return TLBrack;
    case EOF:
        return TEof;
    case '\'':
        idnt[0] = '\'';
        idnt[1] = fgetc(fin);
        idnt[2] = '\'';
        idnt[3] = 0;
        if (fgetc(fin)!='\'')
            die("syntax error, invalid char token");
        return TTokchr;
    }
    p = idnt;
    while (istok(c)) {
        *p++ = c;
        if (p-idnt >= IdntSz-1)
            die("identifier too long");
        c = fgetc(fin);
    }
    if (p == idnt)
        die("unknown token");
    *p = 0;
    if (strcmp(idnt, "%")==0)
    if (c=='{')
        return TLL;
    ungetc(c, fin);
    for (n=0; words[n].name; n++)
        if (strcmp(idnt, words[n].name) == 0)
            return words[n].tok;
    return TIdnt;
}

char *
cpycode()
{
    int c, nest, in, len, pos;
    char *s;

    len = 64;
    s = yalloc(len+1, 1);
    s[0] = '{';
    pos = 1;
    nest = 1;
    in = 0;

    while (nest) {
        c = nextchar()
        if (in) {
            if (c == in)
            if (s[pos-1] != '\\')
                in = 0;
        } else {
            if (c == '"' || c == '\'') // "
                in = c;
            if (c == '{')
                nest++;
            if (c == '}')
                nest--;
            if (c == EOF)
                die("syntax error, unclosed code block");
            if (c == '\n')
                lineno++;
        }
        if (pos>=len)
        if (!(s=realloc(s, len=2*len+1)))
            die("out of memory");
        s[pos++] = c;
    }
    s[pos] = 0;
    return s;
}

fun gettype(typ: string)
{
    int tk;

    if nexttk() == TLangle
    match tk {
    | TLangle =>
        match nexttk() {
        | TIdnt(s) =>
            if (nexttk()!=TRangle)
                die("syntax error, unclosed <");
        | _ => die("syntax error, ident expected after <")

        if (nexttk()!=TIdnt)

        strcpy(type, idnt);

        return nexttk();
    } else {
        type[0] = 0;
        return tk;
    }
}

Sym
findsy(char *name, int add)
{
    int n;

    for (n=0; n<nsy; n++) {
        if (n == ntk) {
            if (name[0]=='\'') {
                if (ntk>=MaxTk)
                    die("too many tokens");
                ntk++;
                strcpy(is[n].name, name);
                return n;
            }
            n = MaxTk;
        }
        if (strcmp(is[n].name, name)==0)
            return n;
    }
    if (add) {
        if (nsy>=MaxTk+MaxNt)
            die("too many non-terminals");
        strcpy(is[nsy].name, name);
        return nsy++;
    } else
        return nsy;
}

void
getdecls()
{
    int tk, prec, p, a, c, c1, n;
    Info *si;
    char type[IdntSz], *s;

    strcpy(is[0].name, "$");
    ntk = 1;
    strcpy(is[StartNT].name, "@start");
    nsy = MaxTk+1;
    sstart = S;
    prec = 0;
    tk = nexttk();
    for (;;)
    switch (tk) {
    case TStart:
        tk = nexttk();
        if (tk!=TIdnt)
            die("syntax error, ident expected after %start");
        sstart = findsy(idnt, 1);
        if (sstart<ntk)
            die("%start cannot specify a token");
        tk = nexttk();
        break;
    case TUnion:
        tk = nexttk();
        if (tk!=TLBrack)
            die("syntax error, { expected after %union");
        fprintf(fout, "#line %d \"%s\"\n", lineno, srca);
        s = cpycode();
        fprintf(fout, "typedef union %s yyunion;\n", s);
        fprintf(fout, "#define YYSTYPE yyunion\n");
        if (fhdr) {
            fprintf(fhdr, "typedef union %s yyunion;\n", s);
            fprintf(fhdr, "#define YYSTYPE yyunion\n");
        }
        free(s);
        doty = 1;
        tk = nexttk();
        break;
    case TLeft:
        p = ++prec;
        a = ALeft;
        goto addtoks;
    case TRight:
        p = ++prec;
        a = ARight;
        goto addtoks;
    case TNonassoc:
        p = ++prec;
        a = ANonassoc;
        goto addtoks;
    case TToken:
        p = 0;
        a = ANone;
    addtoks:
        tk = gettype(type);
        while (tk==TIdnt || tk==TTokchr) {
            si = 0;
            n = findsy(idnt, 0);
            if (n>=MaxTk && n<nsy)
                die("non-terminal redeclared as token");
            if (n==nsy) {
                if (ntk>=MaxTk)
                    die("too many tokens");
                n = ntk++;
            }
            si = &is[n];
            strcpy(si->name, idnt);
            strcpy(si->type, type);
            si->prec = p;
            si->assoc = a;
            tk = nexttk();
        }
        break;
    case TType:
        tk = gettype(type);
        if (type[0]==0)
            die("syntax error, type expected");
        while (tk==TIdnt) {
            si = 0;
            n = findsy(idnt, 1);
            if (n<ntk)
                die("token redeclared as non-terminal");
            if (n==nsy) {
                nsy++;
            }
            si = &is[n];
            strcpy(si->name, idnt);
            strcpy(si->type, type);
            tk = nexttk();
        }
        break;
    case TLL:
        fprintf(fout, "#line %d \"%s\"\n", lineno, srca);
        for (;;) {
            c = fgetc(fin);
            if (c == EOF)
                die("syntax error, unclosed %{");
            if (c == '%') {
                c1 = fgetc(fin);
                if (c1 == '}') {
                    fputs("\n", fout);
                    break;
                }
                ungetc(c1, fin);
            }
            if (c == '\n')
                lineno++;
            fputc(c, fout);
        }
        tk = nexttk();
        break;
    case TPP:
        return;
    case TEof:
        die("syntax error, unfinished declarations");
    default:
        die("syntax error, declaration expected");
    }
}

void
getgram()
{
    extern char *retcode;
    int tk;
    Sym hd, *p, s;
    Rule *r;

    for (;;) {
        tk = nexttk();
        if (tk==TPP || tk==TEof) {
            if (sstart==S)
                die("syntax error, empty grammar");
            r = &rs[nrl++];
            r->lhs = StartNT;
            r->rhs[0] = sstart;
            r->rhs[1] = 0;
            r->rhs[2] = S;
            r->act = retcode;
            qsort(rs, nrl, sizeof rs[0], rcmp);
            return;
        }
        if (tk!=TIdnt)
            die("syntax error, production rule expected");
        if (nexttk()!=TColon)
            die("syntax error, colon expected after production's head");
        hd = findsy(idnt, 1);
        if (sstart==S)
            sstart = hd;
        do {
            if (nrl>=MaxRl-1)
                die("too many rules");
            r = &rs[nrl++];
            r->lhs = hd;
            r->act = 0;
            p = r->rhs;
            while ((tk=nexttk())==TIdnt || tk==TTokchr || tk==TPrec) {
                if (tk==TPrec) {
                    tk = nexttk();
                    if (tk!=TIdnt
                    || (s=findsy(idnt, 0))>=ntk)
                        die("token expected after %prec");
                    r->prec = is[s].prec;
                    continue;
                }
                s = findsy(idnt, 1);
                *p++ = s;
                if (s<ntk && is[s].prec>0)
                    r->prec = is[s].prec;
                if (p-r->rhs >= MaxRhs-1)
                    die("production rule too long");
            }
            *p = S;
            if (tk==TLBrack) {
                r->actln = lineno;
                r->act = cpycode();
                tk = nexttk();
            }
        } while (tk==TBar);
        if (tk!=TSemi)
            die("syntax error, ; or | expected");
    }
}

void
actout(Rule *r)
{
    long l;
    int i, ar;
    char c, *p, *ty, tya[IdntSz];

    ar = slen(r->rhs);
    p = r->act;
    i = r->actln;
    if (!p)
        return;
    while ((c=*p++))
    switch (c) {
    case '\n':
        i++;
    default:
        fputc(c, fout);
        break;
    case '$':
        c = *p++;
        if (c == '$') {
            fprintf(fout, "yyval");
            if (doty) {
                ty = is[r->lhs].type;
                if (!ty[0]) {
                    lineno = i;
                    die("$$ has no type");
                }
                fprintf(fout, ".%s", ty);
            }
        }
        else if (c == '<') {
            ty = tya;
            while (istok(*p) && ty-tya<IdntSz-1)
                *ty++ = *p++;
            *ty = 0;
            if (*p++!='>') {
                lineno = i;
                die("unclosed tag field");
            }
            ty = tya;
            c = *p++;
            if (c == '$') {
                fprintf(fout, "yyval.%s", ty);
            } else {
                if (!isdigit(c)) {
                    lineno = i;
                    die("number or $ expected afer tag field");
                }
                goto readnum;
            }
        }
        else if (isdigit(c)) {
            ty = 0;
        readnum:
            l = strtol(p-1, &p, 10);
            if (l > ar) {
                lineno = i;
                die("invalid $n");
            }
            fprintf(fout, "ps[%d].val", (int)l);
            if (doty) {
                if (!ty && l>0)
                    ty = is[r->rhs[l-1]].type;
                if (!ty || !ty[0]) {
                    lineno = i;
                    die("$n has no type");
                }
                fprintf(fout, ".%s", ty);
            }
        }
        else {
            fputc('$', fout);
            fputc(c, fout);
        }
    }
    fputs("\n", fout);
}

fun codeout(fout: File.t, code0: string, code1: string)
{
    extern char *code0[], *code1[];
    char **p;
    Rule *r;
    int n, c;

    fout.println(code0)
    for r@n <- g_yacc.rs {
        fout.println(f"\tcase {n}:")
        fout.println(f"#line {r.actln} \"{}\"", r.actln, srca);
        actout(r);
        fputs("\t\tbreak;\n", fout);
    }
    for (p=code1; *p; p++)
        fputs(*p, fout);
    fprintf(fout, "#line %d \"%s\"\n", lineno, srca);
    while ((c=fgetc(fin))!=EOF)
        fputc(c, fout);
}

fun init(argv: string list)
{
    var opts = yacc_options_t {vf = false, df=false, prefix="y"}

    fun process_opts_(argv: string list): void
    {
    | "-v" :: rest => opts.vf = true; process_opts_(rest)
    | "-d" :: rest => opts.df = true; process_opts_(rest)
    | "-b" :: pref :: rest => opts.prefix = pref; process_opts_(rest)
    | grammar :: [] when !grammar.startswith("-") =>
        opts.grammar = grammar
    | _ =>
        File.stderr.println("usage: myacc [-v] [-d] [-b file_prefix] grammar")
        throw YaccExit(1)
    }

    val fin = File.open(opts.grammar, "rb")
    if (strlen(pref) + 10 > sizeof buf)
        die("-b prefix too long");
    val fout = File.open(f"{opts.prefix}.tab.c", "wt")
    val fgrm = File.
    if (vf) {
        sprintf(buf, "%s.output", pref)
        fgrm = fopen(buf, "w");
    }
    if (df) {
        sprintf(buf, "%s.tab.h", pref)
        fhdr = fopen(buf, "w");
        if (fhdr) {
            fprintf(fhdr, "#ifndef Y_TAB_H_\n")
            fprintf(fhdr, "#define Y_TAB_H_\n")
        }
    }
    if (!fin || !fout || (!fgrm && vf) || (!fhdr && df)) {
        die("cannot open work files")
    }
}

/* Glorious macros.
    |sed 's|.*|"&\\n",|'
*/

val retcode = "\t\tyyval = ps[1].val; return 0;"

val code0 = "
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;

int
yyparse()
{
   enum {
       StackSize = 100,
       ActSz = sizeof yyact / sizeof yyact[0],
   };
   struct {
       YYSTYPE val;
       int state;
   } stk[StackSize], *ps;
   int r, h, n, s, tk;
   YYSTYPE yyval;

   ps = stk;
   ps->state = s = yyini;
   tk = -1;
loop:
   n = yyadsp[s];
   if (tk < 0 && n > -yyntoks)
       tk = yytrns[yylex()];
   n += tk;
   if (n < 0 || n >= ActSz || yychk[n] != tk) {
       r = yyadef[s];
       if (r < 0)
           return -1;
       goto reduce;
   }
   n = yyact[n];
   if (n == -1)
       return -1;
   if (n < 0) {
       r = - (n+2);
       goto reduce;
   }
   tk = -1;
   yyval = yylval;
stack:
   ps++;
   if (ps-stk >= StackSize)
       return -2;
   s = n;
   ps->state = s;
   ps->val = yyval;
   goto loop;
reduce:
   ps -= yyr1[r];
   h = yyr2[r];
   s = ps->state;
   n = yygdsp[h] + s;
   if (n < 0 || n >= ActSz || yychk[n] != yyntoks+h)
       n = yygdef[h];
   else
       n = yyact[n];
   switch (r) {
"

val code1 = "
   }
   goto stack;
}"

fun main()
{
    try {
        init(Sys.arguments())
        getdecls()
        getgram()
        ginit()
        stgen()
        tblgen()
        stdump()
        actgen()
        tblout()
        codeout()

        if g_yacc.sr_conflicts {
            File.stderr.println(f"{g_yacc.sr_conflicts} shift/reduce conflicts")
        }
        if g_yacc.rr_conflicts {
            File.stderr.println(f"{g_yacc.rr_conflicts} reduce/reduce conflicts")
        }
    } catch {
    | YaccExit _ => {}
    | Fail (str) => println(str)
    }
}

main()
