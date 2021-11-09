/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

from UTest import *
import Re

TEST("Re.simple", fun()
{
    val reg_ho = Re.compile(r"h.*o")
    val reg_e  = Re.compile(r"e")

    EXPECT_EQ(`reg_ho.fullmatch("hello")`, true)
    EXPECT_EQ(`reg_e.fullmatch("hello")`, false)
    EXPECT_EQ(`reg_ho.prefixmatch("hello").issome()`, true)
    EXPECT_EQ(`reg_e.prefixmatch("hello").issome()`, false)
    EXPECT_EQ(`reg_e.find("hello").issome()`, true)
})

TEST("Re.submatch_extraction", fun()
{
    val bards = "Mockles! Fent on silpen tree,\nBlockards three a-feening,\nMockles, what silps came to thee\nIn thy pantry dreaming?"
    val se_reg = Re.compile(r"(\b[E-Te-t]+\b)")
    val r = Re.compile(r"[.,!? ]([^g-p\s]+)[.,!? ]")
    EXPECT_EQ(`r.find(bards)`, Some([| (23, 29), (24, 28) |]))
    EXPECT_EQ(`se_reg.find(bards)`, Some([| (9, 13), (9, 13) |]))
})

TEST("Re.find", fun()
{
    val str = "Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery."
    val letters = Re.compile(r"\a+")
    val cword = Re.compile(r"c\a+")
    val digits = Re.compile(r"\d+")
    EXPECT_EQ(`letters.prefixmatch(str)`, Some([| (0, 5) |]))
    EXPECT_EQ(`cword.find_str(str, ignorecase=true)`, Some([|"Cheburashka"|]))
    EXPECT_EQ(`digits.find_str("The moon is 384467 kilometers distant.")`, Some([| "384467" |]))
})

TEST("Re.findall", fun()
{
    val fullstring_re = Re.compile(r".*")
    EXPECT_EQ(`fullstring_re.findall_str("The hooves clattered.\nAs if singing:\n— Crib.\nGrab.\nGrub.\nGruff.\n", multiline=true)`,
            [| "The hooves clattered.";"As if singing:";"— Crib.";"Grab.";"Grub.";"Gruff." |])
})

TEST("Re.general_match", fun()
{
    val me = Re.compile(r"\b(?:I'm|I am)\b")
    EXPECT_EQ(`me.find_str("Hello, I'm Cornelius")`, Some([| "I'm" |]))
})

TEST("Re.replace", fun()
{
    val str = "If you'll find a Talker bird, please report me, Gromozeka@Chumaroza.org."
    val email = Re.compile(r"(\w[\w-\.]*)@(\w+)\.(\w+)")
    val digits = Re.compile(r"\d+")
    EXPECT_EQ(`email.replace(str, r"\1 from \2")`,
        "If you'll find a Talker bird, please report me, Gromozeka from Chumaroza.")
    EXPECT_EQ(`digits.replace("2-12-85-06", r"(\0)")`, "(2)-(12)-(85)-(06)")
})

TEST("Re.groups", fun()
{
    val assign_re = Re.compile(r"Variable ([\a_]\w+) is equal to (0x[0-9a-fA-F]+).")
    val (name, value) = match assign_re.find_str("Variable len_in_pieces is equal to 0x509F934.") {
        | Some(arr) => (arr[1], arr[2])
        | _ => ("noname", "none")
        }
    EXPECT_EQ(`name`, "len_in_pieces")
    EXPECT_EQ(`value`, "0x509F934")
})

TEST("Re.exception", fun()
{
    EXPECT_THROWS(`fun() {ignore(Re.compile(r"h.*o["))}`, BadArgError)
})
