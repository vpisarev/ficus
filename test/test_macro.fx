/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// macro-1: tests for the declarative macro engine (expansion, hygiene, the
// @file/@line/@string primitives) and the backtick-free assert_ / EXPECT_*_
// stdlib macros built on top of it.

from UTest import *

// a plain expression macro. NOTE: 'x' is used twice, so the argument is
// evaluated twice -- that is the documented macro-author responsibility; the
// single-evaluation macros (EXPECT_*_) below show the safe pattern.
macro sqr_(x: @expr): int = x*x

// expose the primitives so we can assert on their exact values
macro srctext_(e: @expr): string = @string(e)
macro callsite_line_(e: @expr): int = @line

TEST("macro.basic", fun() {
    EXPECT_EQ(sqr_(6), 36)
    EXPECT_EQ(sqr_(2 + 3), 25)          // (2+3)*(2+3), still 25
})

// macros overload by arity, exactly like functions (macro-1 variant B)
macro pick_(a: @expr): int = a
macro pick_(a: @expr, b: @expr): int = a * b

TEST("macro.arity_overload", fun() {
    EXPECT_EQ(pick_(7), 7)
    EXPECT_EQ(pick_(6, 7), 42)
})

TEST("macro.hygiene", fun() {
    // a template-introduced binder 't' must NOT capture the caller's 't':
    // addt5_ has its own internal 'val t = 5', so the result is 100+5, not 5+5.
    val t = 100
    EXPECT_EQ(addt5_(t), 105)
})

// defined after use on purpose (macros are module-scoped, visible everywhere)
macro addt5_(e: @expr): int {
    val t = 5
    e + t
}

TEST("macro.single_evaluation", fun() {
    // EXPECT_EQ_ must evaluate each argument exactly once
    var calls = 0
    fun eff(): int { calls += 1; 42 }
    EXPECT_EQ_(eff(), 42)
    EXPECT_EQ(calls, 1)
})

TEST("macro.string_primitive", fun() {
    // @string reproduces the argument's exact source span, verbatim
    EXPECT_EQ(srctext_(1 + 2*3), "1 + 2*3")
    EXPECT_EQ(srctext_(foo(a, b)), "foo(a, b)")
    val s = srctext_("hi")
    EXPECT_EQ(s, "\"hi\"")
})

TEST("macro.line_is_call_site", fun() {
    // @line is the call site's line, not the macro definition's: two calls on
    // consecutive lines differ by exactly 1.
    val l1 = callsite_line_(0)
    val l2 = callsite_line_(0)
    EXPECT_EQ(l2, l1 + 1)
})

TEST("macro.assert_pass", fun() {
    assert_(1 + 1 == 2)
    assert_(true)
    EXPECT_EQ(1, 1)                      // reached => no throw above
})

TEST("macro.assert_throw_message", fun() {
    var caught = "", threw = false
    try {
        assert_(2 * 2 == 5)
    } catch {
        | AssertError(m) => threw = true; caught = m
    }
    EXPECT_EQ(threw, true)
    // the message carries the USER file:line (not Builtins.fx) and the source text
    EXPECT_EQ(caught.contains("test_macro.fx:"), true)
    EXPECT_EQ(caught.contains("2 * 2 == 5"), true)
    EXPECT_EQ(caught.contains("violation"), true)
})

TEST("macro.nested_outermost_site", fun() {
    // a macro whose template calls another macro: @string/@line report the
    // OUTERMOST (this) call site
    EXPECT_EQ(outer_src_(3 + 4), "3 + 4")
})

macro inner_src_(e: @expr): string = @string(e)
macro outer_src_(e: @expr): string = inner_src_(e)
