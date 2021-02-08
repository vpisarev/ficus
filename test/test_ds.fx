from UTest import *
import Set
import Map

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
    EXPECT_EQ(d12.size, 6)

    val u12 = s1.union(s2)
    EXPECT_EQ(u12.list(), [: -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 100 :])
    EXPECT_EQ(u12.size, 11)
    EXPECT_EQ(u12.minelem(), -3)
    EXPECT_EQ(u12.maxelem(), 100)

    val i12 = s2.intersect(s1)
    EXPECT_EQ(i12.list(), [: -2, -1, 4 :])
    EXPECT_EQ(i12.size, 3)

    val fold sum0 = 0 for i <- u12.list() {sum0 + i}
    val sum1 = u12.foldl(fun (i: int, s: int) {s + i}, 0)
    val sum2 = u12.foldr(fun (i: int, s: int) {s + i}, 0)
    EXPECT_EQ(sum1, sum0)
    EXPECT_EQ(sum2, sum0)
    EXPECT_EQ(u12.map(fun (i: int) {i*i}), [: 9, 4, 1, 1, 4, 9, 16, 25, 36, 49, 10000 :])

    val s1 = Set.from_list([: "this", "is", "a", "very", "simple", "test", "for", "an", "implementation", "of", "binary", "set" :], scmp)
    EXPECT_EQ(s1.list(), [: "a", "an", "binary", "for", "implementation", "is", "of", "set", "simple", "test", "this", "very" :])
})

TEST("ds.map", fun()
{
    fun cmp(a: 't, b: 't) = a <=> b
    val scmp = (cmp: (string, string)->int)

    type si_map = (string, int) Map.map_t

    val poem =
    "The first day of Christmas,
    My true love sent to me
    A partridge in a pear tree.

    The second day of Christmas,
    My true love sent to me
    Two turtle doves, and
    A partridge in a pear tree.

    The third day of Christmas,
    My true love sent to me
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The fourth day of Christmas,
    My true love sent to me
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The fifth day of Christmas,
    My true love sent to me
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The sixth day of Christmas,
    My true love sent to me
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The seventh day of Christmas,
    My true love sent to me
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The eighth day of Christmas,
    My true love sent to me
    Eight maids a-milking,
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The ninth day of Christmas,
    My true love sent to me
    Nine drummers drumming,
    Eight maids a-milking,
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The tenth day of Christmas,
    My true love sent to me
    Ten pipers piping,
    Nine drummers drumming,
    Eight maids a-milking,
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The eleventh day of Christmas
    My true love sent to me
    Eleven ladies dancing,
    Ten pipers piping,
    Nine drummers drumming,
    Eight maids a-milking,
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree.

    The twelfth day of Christmas
    My true love sent to me
    Twelve fiddlers fiddling,
    Eleven ladies dancing,
    Ten pipers piping,
    Nine drummers drumming,
    Eight maids a-milking,
    Seven swans a-swimming,
    Six geese a-laying,
    Five gold rings,
    Four colly birds,
    Three French hens,
    Two turtle doves, and
    A partridge in a pear tree."

    val words = poem.tokens(fun (c: char) {c.isspace() || c == '.' || c == ','})
    val fold wcounter = (Map.empty(scmp) : si_map) for w <- words {
        wcounter.add(w, wcounter.find(w, 0)+1)
    }

    val ll = [: ("A", 12), ("Christmas", 12), ("Eight", 5), ("Eleven", 2),
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
        ("tree", 12), ("true", 12), ("turtle", 11), ("twelfth", 1) :]

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
    val total_words = wcounter.foldl(fun (_: string, ci: int, c: int) {c + ci}, 0)

    EXPECT_EQ(total_words, total_words_ref)

    val fold (wcounter_odd, ll_odd) = (wcounter, ([]: (string, int) list))
        for (w, c) <- ll {
            if c % 2 == 0 {(wcounter_odd.remove(w), ll_odd)}
            else {(wcounter_odd, (w, c) :: ll_odd)}
        }

    EXPECT_EQ(wcounter_odd.list(), ll_odd.rev())
})
