/*
    WP-E / D3 — T-res positive suite. Locks overload-resolution behavior so any
    change flips a case deliberately (a failing assertion here == a reviewed
    decision, not an accident).

    Self-asserting program (no UTest, no compiler-internal imports): it prints
    each check and throws on the first wrong value, so the fxtest O0/O3
    differential doubles as an assertion at both opt levels.

    resolve-1 status: resolution is collect->rank->commit (least-generic viable
    candidate wins; ties error at fully-determined sites and fall back to
    env-order at under-constrained ones). FB-016 is fixed and locked below;
    FB-007's S2 shape is fixed and locked below (mini-cplx, CplxHelper.fx).
    resolve-2 status: [] is typed TypVarCollection, which un-fenced FB-007's
    S3 shape (free []-accumulator + over-general scalar-left operator) below.

    See docs/overload_resolution_proposal_v2.md, docs/resolve1_surgery_brief.md
    and docs/wpe_tests_report.md.
*/
import ResHelper
from ResHelper import *

var fails = 0
fun check(name: string, got: int, want: int): void {
    if got == want { println(f"ok    {name} = {got}") }
    else { fails += 1; println(f"FAIL  {name}: got {got}, want {want}") }
}

// ---------------------------------------------------------------------------
// PASSING behavior (correct today; must stay correct through the surgery)
// ---------------------------------------------------------------------------

// (1) Overloads differing only in keyword arguments are distinguished.
fun kw(~a: int): int = a * 10
fun kw(~a: int, ~b: int): int = a + b
check("kw.one_key", kw(a=5), 50)
check("kw.two_keys", kw(a=5, b=3), 8)

// (2) Constructor resolution under an expected type.
type box_t = Empty | Full: int
val boxed: box_t = Full(7)
check("ctor.under_expected", (match boxed { | Full(v) => v | Empty => -1 }), 7)

// (3) A concrete overload declared AFTER a generic one wins (env order happens
//     to agree with specificity here). Locks the common good case.
fun good(x: 't): int = 0
fun good(x: int): int = 1
check("concrete_last_wins", good(5), 1)

// (4) Cross-module: module-qualified operator call via the mangled name is the
//     working disambiguation escape hatch (Q4). Both the operator form and the
//     ResHelper.__mul__ qualified form select the rvec_t operator.
val v = rvec_t {x=2, y=3}
check("op.infix", v * v, 13)
check("op.qualified_mangled", ResHelper.__mul__(v, v), 13)

// (5) E0 / notes-#8: a generic function in another module, using ==, sees THIS
//     module's == overload for a locally-defined type when instantiated here.
type color_t = Red | Green | Blue
operator == (a: color_t, b: color_t): bool =
    (a.__tag__ == b.__tag__)
check("xmodule.instance_sees_local_eq",
      (if ResHelper.rmem(Green, [:: Red, Green, Blue]) {1} else {0}), 1)

// ---------------------------------------------------------------------------
// FENCED — behavior that is WRONG today; flips when the surgery lands
// ---------------------------------------------------------------------------

// FB-016 FIXED by resolve-1: least-generic ranking picks the concrete overload
// regardless of declaration order. Before the surgery the generic identity won
// here (env-list is most-recent-first) and this yielded 5.
fun bad(x: int): int = x + 1
fun bad(x: 't): 't = x
check("FB016_fixed.concrete_beats_generic", bad(5), 6)

// ---------------------------------------------------------------------------
// FB-007 shapes via the mini-cplx module (CplxHelper.fx mirrors the fenced
// lib/Complex.fx:15-44 operator shapes; its operators are ninja names, so the
// plain `import CplxHelper` above... see below -- imported here explicitly)
// ---------------------------------------------------------------------------
import CplxHelper
from CplxHelper import *

// S2-shape (FB-007): `'t cplx * int` on a fully-determined receiver must pick
// CplxHelper's `*('t cplx, int)`, not the array `__mul__` broadcast. resolve-1
// ranks the viable set, so this holds regardless of import/declaration order.
val c = ref (cplx(1, 1))
*c *= 2
check("fb007.s2_cplx_times_int", c->re + c->im, 4)

// FB-007 S3-shape, UNFENCED by resolve-2 (TypVarCollection). The fold
// accumulator `prog` is initialized with `[]`, whose type is now
// TypVarCollection ("some list/rrbvec/array") instead of a fully free var.
// At `[:: x*x] + prog` the over-general `+('t, 't cplx)` (CplxHelper,
// mimicking lib/Complex.fx) dies at the viability trial -- a collection-var
// cannot become `'t cplx` -- so only the list-concat `+('t list, 't list)`
// remains and the call resolves correctly even though it is still
// under-constrained. (Before resolve-2 the env-order fallback picked the
// most recently imported cplx candidate, inferring `prog: int list cplx`,
// and `.rev()` failed -- the FromOnnx.fx:1012 / vision_classify.fx failure
// in miniature.) The true deferral of under-constrained calls is still
// session-2 scope; TypVarCollection removes this particular collision class.
fun build(l: int list): int list {
    val fold prog = [] for x <- l { prog = [:: x*x] + prog }
    prog.rev()
}
check("fb007.s3_free_accumulator", build([:: 1, 2, 3]).hd(), 1)

println(f"\ntest_resolve: {fails} failure(s)")
if fails > 0 { throw Fail("test_resolve failed") }
