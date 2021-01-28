from UTest import *

TEST("Basic.literals", fun() {
    val a = 1
    val b = -1
    val c = 3.14
    EXPECT_EQ(a+b, 0)
    EXPECT_NEAR(c-0.14, 3.0, 0.001)
})

test_run_all("")
