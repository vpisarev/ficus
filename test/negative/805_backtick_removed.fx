// expect: the backtick `...` context-capture notation was removed
// macro-1: backtick capture is gone; assert/EXPECT_*/ASSERT_* are macros that
// grab the call site + source text via @file/@line/@string, so a call is direct.
val cond = 1 + 1 == 2
assert(`cond`)
