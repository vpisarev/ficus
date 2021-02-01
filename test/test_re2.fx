from UTest import *
import Re2

TEST("Re2 simple", fun() 
    {
        val reg_ho = Re2.compile(r"h.*o")
        val reg_e  = Re2.compile(r"e")

        EXPECT_EQ(Re2.full_match("hello", reg_ho), true)
        EXPECT_EQ(Re2.full_match("hello", reg_e), false)
        EXPECT_EQ(Re2.partial_match("hello", reg_ho), true)
        EXPECT_EQ(Re2.partial_match("hello", reg_e), true)
    })

TEST("Re2 submatch extraction", fun() 
    {
        val bards = "Mockles! Fent on silpen tree,\n Blockards three a-feening,\n Mockles, what silps came to thee\n In thy pantry dreaming?"

        val se_reg = Re2.compile(r"(\b[E-Te-t]+\b)", Re2.options_t {posix_syntax = true, word_boundary = true})
        EXPECT_EQ(Re2.partial_match_n(bards, r"[.,!? ]([^g-p^\s]+)[.,!? ]"), (true, [(24, 28)]))
        EXPECT_EQ(Re2.partial_match_n_str(bards, se_reg), (true, ["Fent"]))
    })

TEST("Re2 consume", fun() 
    {
        EXPECT_EQ(Re2.consume_n("Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, r"([[:alpha:]]+)"), (true, 5, [(0, 5)]))
        EXPECT_EQ(Re2.consume_n_str("Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, r"([[:alpha:]]+)"), (true, 5, ["Eagle"]))
        EXPECT_EQ(Re2.find_and_consume_n_str("The moon is 384467 kilometers distant.", 0, r"(\d+)"), (true, 18, ["384467"]))
    })

TEST("Re2 findall", fun() 
    {
        val fullstring_re = Re2.compile(r"^.*$",Re2.options_t {posix_syntax = true, one_line = false})
        EXPECT_EQ(Re2.findall_str("The hooves clattered.\nAs if singing:\n— Crib.\nGrab.\nGrub.\nGruff.\n",fullstring_re), (true, ["The hooves clattered.";"As if singing:";"— Crib.";"Grab.";"Grub.";"Gruff."]))
    })

TEST("Re2 general match", fun() 
    {
        val fullstring_re = Re2.compile(r"^.*$",Re2.options_t {posix_syntax = true, one_line = false})
        EXPECT_EQ(Re2.general_match_str("Hello I'm Cornelius", r"(I.m)", 6, 9, Re2.anchor_t {anchor_both=true}), (true, ["I'm"]))
    })

TEST("Re2 replace", fun()
    {

        EXPECT_EQ(Re2.replace("If you'll find a Talker bird, please report me, Gromozeka@Chumaroza.org.", r"(\w+)@(\w+)\.(\w+)", r"\1 from \2"), (true, "If you'll find a Talker bird, please report me, Gromozeka from Chumaroza."))
        EXPECT_EQ(Re2.global_replace("2-12-85-06", r"\d+", r"(\0)"), (true, "(2)-(12)-(85)-(06)"))
    })

TEST("Re2 named groups", fun()
    {
        fun assoc_unwrap(r: int?): int
        {
            match r
            {
                | Some(x) => x
                | _ => -1
            }
        }

        val ngroup = Re2.compile(r"Variable (?P<varname>\w*) is equal to (?P<varval>0x[[:xdigit:]]*).") 
        val (_,named_groups) =  Re2.full_match_n_str("Variable len_in_pieces is equal to 0x509F934.", ngroup)
        val group_names = Re2.named_capturing_groups(ngroup)
        EXPECT_EQ(group_names, [:("varname",1), ("varval",2):])
        EXPECT_EQ(named_groups[assoc_unwrap(List.assoc_opt(group_names, "varname")) - 1], "len_in_pieces")
        EXPECT_EQ(named_groups[assoc_unwrap(List.assoc_opt(group_names, "varval")) - 1], "0x509F934")
    })

TEST("Re2 exception", fun()
    {
        var exception_text = ""
        println("Exception:")
        try 
        {
            println(Re2.full_match("hello", r"h.*o[")) 
        }
        catch 
        {
            | Re2.BadRegexp(s) => println("Expected exception. It's ok.");exception_text = s
        }
        EXPECT_EQ(exception_text, "missing ]: [")
    })