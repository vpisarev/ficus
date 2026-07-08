/*
    WP-E / D1 unit tests for the generality comparator
    (compare_typ_generality / compare_fun_generality) added to Ast_typecheck.fx.

    These tests are the "contract" the later overload-resolver surgery session
    must keep (or deliberately, loudly break). They call the comparator DIRECTLY
    on hand-built typ_t values, so the file imports the compiler as a library:

        bin/ficus -run -I compiler test/test_gencmp.fx

    (fxtest passes `-I compiler` for this entry; see tools/fxtest/manifest.toml.)

    compare_typ_generality is pure structural: it only uses unskolemize_typ (a
    local tree walk) and maybe_unify, never id_info on these inputs, so no global
    compiler state beyond get_id() is needed. Skolems ('t, 'u) are modelled as
    opaque constants TypApp([], id) exactly as skolemize_fun_sig freezes them.

    Result of compare_typ_generality(t1, .., t2, ..) describes t1 RELATIVE TO t2:
      MoreGeneric  - t1 accepts strictly more than t2 (t1 is the fallback)
      LessGeneric  - t1 is strictly more specific (the ranking WINNER)
      EqGeneric    - mutually applicable (ambiguous: "qualify the call")
      IncompGeneric- overlapping but unordered (ambiguous: needs a disambiguator)
*/
import Ast, Ast_typecheck
from Ast import *
from Ast_typecheck import *

fun gc2str(c: gen_cmp_t) = match c {
    | MoreGeneric => "MoreGeneric" | LessGeneric => "LessGeneric"
    | EqGeneric => "EqGeneric"     | IncompGeneric => "IncompGeneric" }

var fails = 0
fun check(name: string, got: gen_cmp_t, want: gen_cmp_t) {
    val g = gc2str(got), w = gc2str(want)
    if g == w { println(f"ok    {name}: {g}") }
    else { fails += 1; println(f"FAIL  {name}: got {g}, want {w}") }
}

val t_id = get_id("t"), u_id = get_id("u")
val skT: typ_t = TypApp([], t_id)          // opaque skolem 't
val skU: typ_t = TypApp([], u_id)          // opaque skolem 'u
val cplx_id = get_id("complex")
fun cplx(t: typ_t) = TypApp([:: t], cplx_id)   // 't complex stand-in

// ---- non-generic beats generic (the fully-skolem limit case of §3) ----------
check("int_vs_t", compare_typ_generality(TypInt, [], skT, [:: t_id]), LessGeneric)
check("t_vs_int", compare_typ_generality(skT, [:: t_id], TypInt, []), MoreGeneric)

// ---- EqGeneric: mutually applicable ----------------------------------------
check("int_vs_int", compare_typ_generality(TypInt, [], TypInt, []), EqGeneric)
check("t_vs_u",     compare_typ_generality(skT, [:: t_id], skU, [:: u_id]), EqGeneric)

// ---- 't vs 't complex: the S2 shape ('t complex * int lost to array __mul__) -
check("t_vs_tcomplex", compare_typ_generality(skT, [:: t_id], cplx(skT), [:: t_id]), MoreGeneric)
check("tcomplex_vs_t", compare_typ_generality(cplx(skT), [:: t_id], skT, [:: t_id]), LessGeneric)
// 't complex (generic arg) vs int complex (concrete arg)
check("tcplx_vs_intcplx", compare_typ_generality(cplx(skT), [:: t_id], cplx(TypInt), []), MoreGeneric)
check("intcplx_vs_tcplx", compare_typ_generality(cplx(TypInt), [], cplx(skT), [:: t_id]), LessGeneric)

// ---- genuinely incomparable: f('t, int) vs f(int, 'u) on (int,int) ----------
val a1 = TypTuple([:: skT, TypInt]), b1 = TypTuple([:: TypInt, skU])
check("Ttup_int_vs_int_Utup", compare_typ_generality(a1, [:: t_id], b1, [:: u_id]), IncompGeneric)

// ---- keyword-record subsumption ---------------------------------------------
// Keyword args travel as an implicit trailing TypRecord; overloads differing
// only in keyword sets meet here. Per proposal §10.Q2 the DEFAULT policy adds NO
// "fewer-defaults-preferred" tie-break, so distinct keyword sets do NOT order ->
// IncompGeneric (ambiguity). Locked as the contract; the surgery session flips
// this ONLY if the corpus (E4/E1) shows a real need for a keyword tie-break.
val nop = ExpNop(noloc), df = default_val_flags()
fun recf(fields: (id_t, typ_t) list, closed: bool) =
    TypRecord(ref ([:: for (n,t) <- fields {(df, n, t, nop)}], closed))
val recA_open  = recf([:: (get_id("a"), TypInt)], false)
val recA_clsd  = recf([:: (get_id("a"), TypInt)], true)
val recAB_clsd = recf([:: (get_id("a"), TypInt), (get_id("b"), TypInt)], true)
check("kw_open_a_vs_closed_ab",   compare_typ_generality(recA_open, [], recAB_clsd, []), IncompGeneric) // FIXME(WP-E Q2)
check("kw_closed_a_vs_closed_ab", compare_typ_generality(recA_clsd, [], recAB_clsd, []), IncompGeneric) // FIXME(WP-E Q2)

// ---- var-form generics: {...} / (...) / ('t ...) / 't [+]  (FB-017 fix) ------
// The rigid side of a trial freezes var-forms into opaque sentinels
// (freeze_varform_typs), so a concrete type now ranks strictly more specific
// than a var-form generic, while identical var-forms stay mutually applicable.
fun vrec(): typ_t = TypVar(ref Some(TypVarRecord))                // {...}
fun vtup(p: typ_t?): typ_t = TypVar(ref Some(TypVarTuple(p)))     // (...) / (p ...)
fun varr(et: typ_t): typ_t = TypVar(ref Some(TypVarArray(et)))    // et [+]

check("rec_vs_varrec", compare_typ_generality(recA_clsd, [], vrec(), []), LessGeneric)
check("varrec_vs_rec", compare_typ_generality(vrec(), [], recA_clsd, []), MoreGeneric)
check("varrec_vs_varrec", compare_typ_generality(vrec(), [], vrec(), []), EqGeneric)
// {...} accepts only records, plain 't accepts everything
check("varrec_vs_t", compare_typ_generality(vrec(), [], skT, [:: t_id]), LessGeneric)
check("t_vs_varrec", compare_typ_generality(skT, [:: t_id], vrec(), []), MoreGeneric)

val tup_ii = TypTuple([:: TypInt, TypInt])
check("tup_vs_vartup", compare_typ_generality(tup_ii, [], vtup(None), []), LessGeneric)
check("vartup_vs_vartup", compare_typ_generality(vtup(None), [], vtup(None), []), EqGeneric)
// ('t ...) (all elements equal) is strictly less generic than (...)
check("ttup_vs_vartup", compare_typ_generality(vtup(Some(skT)), [:: t_id], vtup(None), []), LessGeneric)
check("vartup_vs_ttup", compare_typ_generality(vtup(None), [], vtup(Some(skT)), [:: t_id]), MoreGeneric)
check("inttup_vs_ttup", compare_typ_generality(vtup(Some(TypInt)), [],
                                               vtup(Some(skT)), [:: t_id]), LessGeneric)

val arr1_int = TypArray(1, TypInt)
check("intarr_vs_tvararr", compare_typ_generality(arr1_int, [], varr(skT), [:: t_id]), LessGeneric)
check("tvararr_vs_intarr", compare_typ_generality(varr(skT), [:: t_id], arr1_int, []), MoreGeneric)
check("vararr_vs_vararr", compare_typ_generality(varr(skT), [:: t_id], varr(skU), [:: u_id]), EqGeneric)
check("intvararr_vs_tvararr", compare_typ_generality(varr(TypInt), [], varr(skT), [:: t_id]), LessGeneric)

// ---- interface direction: FENCED --------------------------------------------
// §3 wants "concrete class less generic than its interface" and "child iface
// less generic than parent". That needs UPCAST-AWARE unification (same_or_parent
// / dvar_ifaces), which lives in the cast logic, NOT in maybe_unify. The pure
// structural comparator therefore reports two distinct nominal types as
// IncompGeneric today. Locked here so it flips LOUDLY when the surgery session
// teaches the comparator about class->interface / child->parent upcasts.
val cls_id = get_id("MyClass"), iface_id = get_id("MyIface")
check("class_vs_iface_FENCED",
      compare_typ_generality(TypApp([],cls_id), [], TypApp([],iface_id), []),
      IncompGeneric) // FIXME(WP-E surgery): should become LessGeneric once upcast-aware

// Note: compare_fun_generality (the deffun_t ref wrapper: skolemization + keyword
// record + constructor return-type rule) is exercised end-to-end by the
// -pr-resolve census on real programs (D2/E1), since building valid deffun_t
// refs by hand needs the full typechecker environment.

println(f"\ntest_gencmp: {fails} failure(s)")
if fails > 0 { throw Fail("test_gencmp failed") }
