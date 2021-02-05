from UTest import *
import Set

TEST("ds.set", fun()
{
    fun cmp(a: 't, b: 't) = a <=> b
    val icmp = (cmp: (int, int)->int)
    val scmp = (cmp: (string, string)->int)

    EXPECT_EQ(icmp(5, 3), 1)
    EXPECT_EQ(scmp("bar", "baz"), -1)
    EXPECT_EQ(scmp("foo", "foo"), 0)

    type intset = int Set.set_t
    type strset = string Set.set_t

    val s1 = Set.from_list([: 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1, -1, -2, -3 :], icmp)
    EXPECT_EQ(s1.list(), [: -3, -2, -1, 1, 2, 3, 4, 5, 6 :])
    val s2 = Set.from_list([: 100, -1, 4, -2, 7 :], icmp)

    val d12 = s1.diff(s2)
    EXPECT_EQ(d12.list(), [: -3, 1, 2, 3, 5, 6 :])

    val u12 = s1.union(s2)
    EXPECT_EQ(u12.list(), [: -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 100 :])
    EXPECT_EQ(u12.minelem(), -3)
    EXPECT_EQ(u12.maxelem(), 100)

    val i12 = s2.intersect(s1)
    EXPECT_EQ(i12.list(), [: -2, -1, 4 :])

    val fold sum0 = 0 for i <- u12.list() {sum0 + i}
    val sum1 = u12.fold_left(fun (i: int, s: int) {s + i}, 0)
    val sum2 = u12.fold_right(fun (i: int, s: int) {s + i}, 0)
    EXPECT_EQ(sum1, sum0)
    EXPECT_EQ(sum2, sum0)
    EXPECT_EQ(u12.map(fun (i: int) {i*i}), [: 9, 4, 1, 1, 4, 9, 16, 25, 36, 49, 10000 :])
})
