/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

from UTest import *
import Set, Map, Hashmap, Hashset

val poem = @text "poem.txt"

TEST("ds.set", fun()
{
    fun cmp(a: 't, b: 't) = a <=> b
    val icmp = (cmp: (int, int)->int)
    val scmp = (cmp: (string, string)->int)

    EXPECT_EQ(icmp(5, 3), 1)
    EXPECT_EQ(scmp("bar", "baz"), -1)
    EXPECT_EQ(scmp("foo", "foo"), 0)

    type intset = int Set.t
    type strset = string Set.t

    val s1 = Set.from_list(icmp, [ 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1, -1, -2, -3 ])
    EXPECT_EQ(s1.list(), [ -3, -2, -1, 1, 2, 3, 4, 5, 6 ])
    val s2 = Set.from_list(icmp, [ 100, -1, 4, -2, 7 ])

    val d12 = s1.diff(s2)
    EXPECT_EQ(d12.list(), [ -3, 1, 2, 3, 5, 6 ])
    EXPECT_EQ(d12.size, 6)

    val u12 = s1.union(s2)
    EXPECT_EQ(u12.list(), [ -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 100 ])
    EXPECT_EQ(u12.size, 11)
    EXPECT_EQ(u12.minelem(), -3)
    EXPECT_EQ(u12.maxelem(), 100)

    val i12 = s2.intersect(s1)
    EXPECT_EQ(i12.list(), [ -2, -1, 4 ])
    EXPECT_EQ(i12.size, 3)

    val fold sum0 = 0 for i <- u12.list() {sum0 + i}
    val sum1 = u12.foldl(fun (i, s) {s + i}, 0)
    val sum2 = u12.foldr(fun (i, s) {s + i}, 0)
    EXPECT_EQ(sum1, sum0)
    EXPECT_EQ(sum2, sum0)
    EXPECT_EQ(u12.map(fun (i) {i*i}), [ 9, 4, 1, 1, 4, 9, 16, 25, 36, 49, 10000 ])
    val phrase = "This is a very simple test for the standard and not \
        so simple implementation of binary set".split(' ', allow_empty=true)
    val refres = ["This", "a", "and", "binary", "for", "implementation", "is", "not",
                    "of", "set", "simple", "so", "standard",
                    "test", "the", "very"]

    val s1 = Set.from_list(scmp, phrase)
    EXPECT_EQ(s1.list(), refres)
})

TEST("dst.hashset", fun()
{
    val phrase = "This is a very simple test for the standard and not \
        so simple implementation of binary set".split(' ', allow_empty=true)
    val refres = ["This", "a", "and", "binary", "for", "implementation", "is", "not",
                    "of", "set", "simple", "so", "standard",
                    "test", "the", "very"]
    val s2 = Hashset.empty(8, "")
    for w <- phrase {s2.add(w)}
    EXPECT_EQ(s2.list().sort((<)), refres)
    EXPECT_EQ(s2.mem("simple") && s2.mem("very"), true)
    s2.remove("simple")
    EXPECT_EQ(s2.mem("this") || s2.mem("complex") || s2.mem("simple"), false)

    val s1 = Hashset.from_list(0, [ 1, 2, 3, 100, 20, 30 ])
    val s2 = Hashset.from_list(5, [ 1, 7, 3, 110, 20, 30 ])
    val s_inter = s1.copy()
    s_inter.intersect(s2)
    val s_union = s1.copy()
    s_union.union(s2)
    EXPECT_EQ(s_inter.list().sort((<)), [ 1, 3, 20, 30 ])
    EXPECT_EQ(s_union.list().sort((<)), [ 1, 2, 3, 7, 20, 30, 100, 110 ])
})

TEST("ds.map", fun()
{
    fun cmp(a: 't, b: 't) = a <=> b
    val scmp = (cmp: (string, string)->int)

    type si_map = (string, int) Map.t

    val words = poem.tokens(fun (c) {c.isspace() || c == '.' || c == ','})
    val fold wcounter = (Map.empty(scmp) : si_map) for w <- words {
        wcounter.add(w, wcounter.find(w, 0)+1)
    }

    val ll = [("A", 12), ("Christmas", 12), ("Eight", 5), ("Eleven", 2),
        ("Five", 8), ("Four", 9), ("French", 10), ("My", 12), ("Nine", 4),
        ("Seven", 6), ("Six", 7), ("Ten", 3), ("The", 12), ("Three", 10),
        ("Twelve", 1), ("Two", 11), ("a", 12), ("a-laying", 7), ("a-milking", 5),
        ("a-swimming", 6), ("and", 11), ("birds", 9), ("colly", 9), ("dancing", 2),
        ("day", 12), ("doves", 11), ("drummers", 4), ("drumming", 4), ("eighth", 1),
        ("eleventh", 1), ("fiddlers", 1), ("fiddling", 1), ("fifth", 1), ("first", 1),
        ("fourth", 1), ("geese", 7), ("gold", 8), ("hens", 10), ("in", 12), ("ladies", 2),
        ("love", 12), ("maids", 5), ("me", 12), ("ninth", 1), ("of", 12), ("partridge", 12),
        ("pear", 12), ("pipers", 3), ("piping", 3), ("rings", 8), ("second", 1), ("sent", 12),
        ("seventh", 1), ("sixth", 1), ("swans", 6), ("tenth", 1), ("third", 1), ("to", 12),
        ("tree", 12), ("true", 12), ("turtle", 11), ("twelfth", 1)]

    EXPECT_EQ(wcounter.list(), ll)

    // An alternative, faster way to increment word counters is to use Map.update() function,
    // where we search for each word just once
    val fold wcounter2 = (Map.empty(scmp) : si_map) for w <- words {
            wcounter2.update(w,
                fun (_: string, ci_opt: int?)
                {
                    | (_, Some(ci)) => ci + 1
                    | _ => 1
                })
        }

    EXPECT_EQ(wcounter2.list(), ll)

    val total_words_ref = fold c=0 for (_, ci) <- ll {c+ci}
    val total_words = wcounter.foldl(fun (_, ci, c) {c + ci}, 0)

    EXPECT_EQ(total_words, total_words_ref)

    val fold wcounter_odd=wcounter, ll_odd=[] for (w, c) <- ll {
            if c % 2 == 0 {(wcounter_odd.remove(w), ll_odd)}
            else {(wcounter_odd, (w, c) :: ll_odd)}
        }

    EXPECT_EQ(wcounter_odd.list(), ll_odd.rev())
})

TEST("ds.hashmap", fun() {
    type si_hash = (string, int) Hashmap.t
    val words = poem.tokens(fun (c) {c.isspace() || c == '.' || c == ','})
    val wcounter = Hashmap.empty(8, "", 0)
    val odd_wcounter = Hashmap.empty(8, "", 0)
    for w@i <- words {
        val idx = wcounter.find_idx_or_insert(w)
        wcounter.table[idx].data += 1
        val odd_idx = odd_wcounter.find_idx_or_insert(w)
        odd_wcounter.table[odd_idx].data += 1
        if odd_wcounter.table[odd_idx].data >= 2 {
            // remove words in the middle of the word collecting process
            // rather than in the end to test how well hashmap stands intensive
            // flow of additions and removals
            odd_wcounter.remove(w)
        }
    }

    val odd_ll = [("Eight", 1), ("Four", 1), ("Six", 1), ("Ten", 1), ("Twelve", 1),
                     ("Two", 1), ("a-laying", 1), ("a-milking", 1), ("and", 1), ("birds", 1),
                     ("colly", 1), ("doves", 1), ("eighth", 1), ("eleventh", 1), ("fiddlers", 1),
                     ("fiddling", 1), ("fifth", 1), ("first", 1), ("fourth", 1), ("geese", 1),
                     ("maids", 1), ("ninth", 1), ("pipers", 1), ("piping", 1), ("second", 1),
                     ("seventh", 1), ("sixth", 1), ("tenth", 1), ("third", 1), ("turtle", 1),
                     ("twelfth", 1)]
    val ll = [("A", 12), ("Christmas", 12), ("Eight", 5), ("Eleven", 2),
        ("Five", 8), ("Four", 9), ("French", 10), ("My", 12), ("Nine", 4),
        ("Seven", 6), ("Six", 7), ("Ten", 3), ("The", 12), ("Three", 10),
        ("Twelve", 1), ("Two", 11), ("a", 12), ("a-laying", 7), ("a-milking", 5),
        ("a-swimming", 6), ("and", 11), ("birds", 9), ("colly", 9), ("dancing", 2),
        ("day", 12), ("doves", 11), ("drummers", 4), ("drumming", 4), ("eighth", 1),
        ("eleventh", 1), ("fiddlers", 1), ("fiddling", 1), ("fifth", 1), ("first", 1),
        ("fourth", 1), ("geese", 7), ("gold", 8), ("hens", 10), ("in", 12), ("ladies", 2),
        ("love", 12), ("maids", 5), ("me", 12), ("ninth", 1), ("of", 12), ("partridge", 12),
        ("pear", 12), ("pipers", 3), ("piping", 3), ("rings", 8), ("second", 1), ("sent", 12),
        ("seventh", 1), ("sixth", 1), ("swans", 6), ("tenth", 1), ("third", 1), ("to", 12),
        ("tree", 12), ("true", 12), ("turtle", 11), ("twelfth", 1)]

    val ll_fh = wcounter.list().sort((<))
    val odd_ll_fh = odd_wcounter.list().sort((<))
    EXPECT_EQ(ll_fh, ll)
    EXPECT_EQ(odd_ll_fh, odd_ll)
    EXPECT_EQ(wcounter.find_opt("doves").value_or(-1), 11)
    EXPECT_EQ(wcounter.find_opt("silver").value_or(-1), -1)
})
