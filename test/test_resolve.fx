/*
    WP-E / D3 — T-res positive suite. Locks CURRENT overload-resolution behavior
    so the later resolver surgery flips each change deliberately (a failing
    assertion here == a reviewed decision, not an accident).

    Self-asserting program (no UTest, no compiler-internal imports): it prints
    each check and throws on the first wrong value, so the fxtest O0/O3
    differential doubles as an assertion at both opt levels.

    Cases marked FIXME(FB-016)/FIXME(FB-007) lock behavior that is WRONG today and
    is expected to change when tranche A of the resolver surgery lands; when it
    does, the corresponding assertion fails loudly and is updated in review.

    See docs/overload_resolution_proposal_v2.md and docs/wpe_tests_report.md.
*/
import ResHelper
from ResHelper import *

var fails = 0
fun check(name: string, got: int, want: int) {
    if got == want { println(f"ok    {name} = {got}") }
    else { fails += 1; println(f"FAIL  {name}: got {got}, want {want}") }
}

// ---------------------------------------------------------------------------
// PASSING behavior (correct today; must stay correct through the surgery)
// ---------------------------------------------------------------------------

// (1) Overloads differing only in keyword arguments are distinguished.
fun kw(~a: int) = a * 10
fun kw(~a: int, ~b: int) = a + b
check("kw.one_key", kw(a=5), 50)
check("kw.two_keys", kw(a=5, b=3), 8)

// (2) Constructor resolution under an expected type.
type box_t = Empty | Full: int
val boxed: box_t = Full(7)
check("ctor.under_expected", (match boxed { | Full(v) => v | Empty => -1 }), 7)

// (3) A concrete overload declared AFTER a generic one wins (env order happens
//     to agree with specificity here). Locks the common good case.
fun good(x: 't) = 0
fun good(x: int) = 1
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

// FIXME(FB-016): greedy first-match. A concrete overload declared BEFORE a
// generic one LOSES to the generic (env-list is most-recent-first). So bad(5)
// calls the generic identity and yields 5; a specificity resolver yields 6.
// When tranche A lands, this assertion fails -> change 5 to 6 and drop the fence.
fun bad(x: int) = x + 1
fun bad(x: 't) = x
check("FIXME_FB016.generic_beats_concrete", bad(5), 5)   // want-after-fix: 6

/*
   FIXME(FB-007): the three generic-`complex` resolution symptoms are NOT
   reproducible in a self-contained mini-class — they require the real
   lib/Complex.fx operators (uncommented) plus the `N.fi` imaginary literal and
   the NN library's generic `+` on `nnop_t list`. Ground-truthed in this WP-E
   (see docs/found_bugs.md FB-007 and docs/wpe_tests_report.md):

   - S2 (operator mis-resolution): with Complex.fx ops uncommented,
       val c = ref (1 + 1.fi); *c *= 2
     resolves `__mul__` to the ARRAY multiply
     (lib/Array.fx:341: "unsupported iteration domain").
   - S3 (inference pollution): compiling examples/vision_classify.fx with those
     ops on infers `nnop_t list Complex.complex` for a plain `nnop_t list`
     (lib/Complex.fx:17), cascading to NN/FromOnnx.fx:1018.

   These flip to pass when tranche A lands; they are fenced by keeping
   lib/Complex.fx:15-44 and examples/fst.fx:125-127 commented in-tree.
*/

println(f"\ntest_resolve: {fails} failure(s)")
if fails > 0 { throw Fail("test_resolve failed") }
