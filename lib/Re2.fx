/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Re2 library wrapper.

    Re2 library must be installed in system, see installation instructions there:
    https://github.com/google/re2/wiki/Install

    Sometimes(E.g.,, on Debian 10) it's also needed to run
        sudo ldconfig
    after installation for creating appropriate soft links for shared libraries.


    See https://github.com/google/re2/wiki/Syntax re2.h of Re2 source for more detailed information about syntax and options.
    Wrapper function signatures have ficus-dependent specialization. They are described there.
*/

pragma "c++", "clib:re2"

exception BadRegexp : string
@ccode
{
    #include <re2/re2.h>
    #include <vector>

    //[TODO] : Exception, declared before @ccode block must be declared in program, translated to C program before this @ccode for avoiding such insets.
    FX_EXTERN_C int _fx_M3Re2FM14make_BadRegexpE1S(fx_str_t* arg0, fx_exn_t* fx_result);

    static int throwRe2Badregexp(const std::string& err)
    {
        fx_exn_t v_0 = {};
        int fx_status = 0;
        fx_str_t werr;

        fx_status = fx_cstr2str(err.c_str(), err.size(), &werr);
        if(fx_status >= 0)
        {
            FX_CALL(_fx_M3Re2FM14make_BadRegexpE1S(&werr, &v_0), _fx_cleanup); //[TODO] : We need C-side infrastacture for user-registered exceptions. Name mangling for exception functions is a root of problem.
            FX_THROW(&v_0, true, _fx_cleanup);
        }
        else
        {
            FX_UPDATE_BT();
        }
    _fx_cleanup: ;
        return fx_status;
    }

    #define FX_RE2_THROW_BADREGEXP(str, catch_label) { fx_status = throwRe2Badregexp((str)); goto catch_label; }

    void fx_re2_free(void* ptr)
    {
        using namespace re2;
        delete static_cast<RE2*>(ptr);
    }

    static void make_sub_args(std::vector<re2::StringPiece>& a_sub_string_pieces, std::vector<re2::RE2::Arg*>& a_sub_args, std::vector<re2::RE2::Arg>& a_sub_args_storage)
    {
        using namespace re2;
        for(size_t arg_num = 0; arg_num < a_sub_args.size(); arg_num++)
        {
            a_sub_args_storage[arg_num] = RE2::Arg(&a_sub_string_pieces[arg_num]);
            a_sub_args[arg_num] = &a_sub_args_storage[arg_num];
        }
    }

    int string_pieces_to_arr(const std::vector<re2::StringPiece>& sub_string_pieces, const char* string_itself, fx_str_t* correction_pattern, fx_arr_t* fx_result)
    {
        const int_ arrdims[1] = {static_cast<int_>(sub_string_pieces.size())};
        int fx_status = fx_make_arr( 1, arrdims, sizeof(int_)+sizeof(int_), 0, 0, 0, fx_result);
        if(fx_status>=0)
        {
            int_* starts_and_ends = (int_*)fx_result->data;              //tuple of 2 * int have size = sizeof(int) + sizeof(int)
            std::map<int_, size_t> indexmap;
            for(size_t piece_num = 0; piece_num < sub_string_pieces.size(); piece_num++)
            {
                starts_and_ends[2 * piece_num    ] = sub_string_pieces[piece_num].begin() - string_itself;
                starts_and_ends[2 * piece_num + 1] = sub_string_pieces[piece_num].end()   - string_itself;
                indexmap[starts_and_ends[2 * piece_num    ]] = 2 * piece_num;
                indexmap[starts_and_ends[2 * piece_num + 1]] = 2 * piece_num + 1;
            }

            size_t cor_sz = 0;
            int_ len = correction_pattern->length;
            const char_* src = correction_pattern->data;
            int_ i = 0;

            std::map<int_, size_t>::iterator iter = indexmap.begin();

            while(iter != indexmap.end())
            {
                while(cor_sz < iter->first)
                {
                    char_ ch = src[i];
                    cor_sz++;
                    cor_sz += ch > 127;
                    cor_sz += ch > 2047;
                    cor_sz += (ch > 65535) & (ch <= 1114111);
                    i++;
                }
                while(iter != indexmap.end() && cor_sz == iter->first)
                {
                    starts_and_ends[iter->second] = i;
                    iter++;
                }
            }
        }
        return fx_status;
    }

}

fun incise(str: string, starts_ends: (int, int)[]): string []
{
    [| for (start, end) <- starts_ends {str[start:end]} |]
}

/////////////////////////////////////////////////////////////////////////////////////////
// Pattern compilation and informational functions //////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////


//    regex_t - precompiled regexp pattern. Use it for increasing speed, when it's
//    needed to check match with same pattern multiple times.
//    Equivalent of RE2 objects.
type regex_t = { handle: cptr; find_r: cptr}

//    replace_pattern_t - precompiled replace pattern. Use it for increasing speed, when it's
//    needed to replace many found occurencies.
type replace_piece_t =
| RPInt: int
| RPString: string

type replace_pattern_t = {pieces: replace_piece_t list; max_subnum: int}

//    options_t - regular expression options.
//    Equivalent of RE2::Options objects. See re2.h for detailed meaning of particular option.
type options_t =
{
    posix_syntax: bool = false;
    longest_match: bool = false;
//    max_mem: int = kDefaultMaxMem;
    literal: bool = false;
    never_nl: bool = false;
    dot_nl: bool = false;
    never_capture: bool = false;
    case_sensitive: bool = true;
    perl_classes: bool = false;
    word_boundary: bool = false;
    one_line: bool = false
}

//    Complile pattern to precompiled regex_t object.
//        pattern - string representation of etalon to be found. It's good idea to use r"" literals.
//            Slashed in in r""-literals are just slashes, they couldn't be part of ficus
//            escape-sequence, therefore double slashes are not required for escape-sequences
//            of regular expressions.
//        options - settings for flexible configuration.
//
//    E.g, pattern for separate words:
//    Re2.compile(r"(\b[:alpha:]+\b)", Re2.options_t {posix_syntax = true, word_boundary = true})
//        word_boundary option adds ability to use r"\b" symbol, but this option is silent, until
//        posix_syntax is switched off.
//
//    See https://github.com/google/re2/wiki/Syntax for full description of regular expressions
//    syntax.
fun compile(pattern: string, options: options_t): regex_t
{
    fun compile_re2(pattern: string, options: options_t): cptr = @ccode
    {
        using namespace re2;
        fx_cstr_t cpattern;
        int fx_status = fx_str2cstr(pattern, &cpattern, 0, 0);
        if(fx_status>=0)
        {
            RE2::Options options_(RE2::Quiet);
            {
                options_.set_posix_syntax(options->posix_syntax);
                options_.set_longest_match(options->longest_match);
                options_.set_literal(options->literal);
                options_.set_never_nl(options->never_nl);
                options_.set_dot_nl(options->dot_nl);
                options_.set_never_capture(options->never_capture);
                options_.set_case_sensitive(options->case_sensitive);
                options_.set_perl_classes(options->perl_classes);
                options_.set_word_boundary(options->word_boundary);
                options_.set_one_line(options->one_line);
            }

            RE2* new_re = nullptr;
            try
            {
                new_re = new RE2(cpattern.data, options_);
            }
            catch (std::bad_alloc& e)
            {
                FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
            }
            if(!new_re->ok())
            {
                std::string errorstr = new_re->error();
                fx_re2_free(new_re);
                FX_RE2_THROW_BADREGEXP(errorstr, fx_cleanup);
            }
            fx_status = fx_make_cptr(new_re, fx_re2_free, fx_result);
        }
    fx_cleanup:
        fx_free_cstr(&cpattern);
        return fx_status;
    }
    regex_t {handle = compile_re2(pattern, options), find_r = compile_re2("("+pattern+")", options)}
}

fun compile(pattern: string): regex_t
{
    val default_options = options_t {}
    compile(pattern, default_options)
}


//    Return original pattern string.
//
//    Equivalent of RE2::pattern
@pure fun string(re: regex_t): string = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    return fx_cstr2str(re_to_apply->pattern().c_str(), re_to_apply->pattern().size(), fx_result);
}

//    Number of sub-matches. Full match isn't counted.
//
//    Equivalent of RE2::NumberOfCapturingGroups
@pure fun number_of_capturing_groups(re: regex_t): int = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    *fx_result = re_to_apply->NumberOfCapturingGroups();
    return FX_OK;
}

//    There is a syntax for giving a name to particular sub-match.
//
//    E.g., pattern r"(?P<name>\w+)@(?P<domain>\w+)\.(?P<country>\w+)"
//    descripts simple e-mail adress. (Note, this is r""-literal).
//    Each part of word have name.
//
//    This function extracts assoc list from sub-match names to sub-match
//    indexes. Therefore, it's possible to get sub-match by name.
//
//    Equivalent of RE2::NamedCapturingGroups
fun named_capturing_groups(re: regex_t): (string, int) list
{
    @pure fun named_capturing_groups_(re: regex_t): (string [], int []) = @ccode
    {
        using namespace re2;
        RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
        if(re_to_apply == nullptr)
            return FX_EXN_NullPtrError;
        size_t sub_amount = re_to_apply->NumberOfCapturingGroups();
        const int_ arrdims[1] = {static_cast<int_>(sub_amount)};
        int fx_status = fx_make_arr( 1, arrdims, sizeof(fx_str_t), 0, 0, 0, &fx_result->t0);
        if(fx_status<0)
        {
            return fx_status;
        }
        fx_status = fx_make_arr( 1, arrdims, sizeof(int_), 0, 0, 0, &fx_result->t1);
        if(fx_status<0)
        {
            fx_free_arr(&fx_result->t0);
            return fx_status;
        }

        int_* indexes = (int_*)fx_result->t1.data;
        fx_str_t* names = (fx_str_t*)fx_result->t0.data;

        size_t arrpos = 0;
        std::map<std::string, int>::const_iterator rator = re_to_apply->NamedCapturingGroups().cbegin();
        while(rator != re_to_apply->NamedCapturingGroups().cend())
        {
            indexes[arrpos] = rator->second;
            fx_status = fx_cstr2str(rator->first.c_str(), rator->first.size(), names+arrpos);
            if(fx_status<0)
            {
                for(size_t name_num = 0; name_num < arrpos; name_num++)
                {
                    fx_free_str(names+arrpos);
                }
                fx_free_arr(&fx_result->t0);
                fx_free_arr(&fx_result->t1);
                break;
            }
            arrpos++;
            rator++;
        }
        return fx_status;
    }
    val (names, indexes) = named_capturing_groups_(re)
    [: for name <- names, index <- indexes {(name, index)} :]
}


//    Extracts assoc list from sub-match indexes to sub-match names.
//
//    Equivalent of RE2::CapturingGroupNames
fun capturing_group_names(re: regex_t): (int, string) list
{
    val lst = named_capturing_groups(re)
    [: for (name, index) <- lst {(index, name)} :]
}

val digit_reg = compile(r"\\[0-9]")

//    Complile replace strings for increasing speed of substitution.
//    See replace for details.
fun compile_replace_pattern(rewrite: string): replace_pattern_t
{
    val (has_subs, ranges) = findall(rewrite, digit_reg)
    val piece_lst =
        if(has_subs)
        {
            val (len_i, _) = size(ranges)
            var pieces_list = []
            var pos = 0
            for incl_num <- 0: len_i
            {
                val (substart,subend) = ranges[incl_num,0]
                val placeholder_num = rewrite[substart+1:substart+2].to_int_or(0)
                pieces_list = RPInt(placeholder_num) :: RPString(rewrite[pos:substart]) :: pieces_list
                pos = subend
            }
            pieces_list = RPString(rewrite[pos:]) :: pieces_list
            fold filtered = [] for piece <- pieces_list
            {
                val ok = match piece { | RPString simple_piece => simple_piece.length()!=0 | RPInt sub_num => true}
                if (ok) {piece::filtered} else {filtered}
            }
        }
        else
        {
            RPString(rewrite) :: []
        }

    val fold max_sub = -1 for piece <- piece_lst
    {
        match piece
        {
            | RPString simple_piece => max_sub
            | RPInt sub_num => max(max_sub, sub_num)
        }
    }
    replace_pattern_t {pieces = piece_lst, max_subnum = max_sub}
}

//    Returns maximal number of submatch met in replace pattern.
//
//    Equivalent of RE2::MaxSubmatch
fun max_submatch(rewrite: replace_pattern_t): int
{
    rewrite.max_subnum
}

//    Check if regexp and replace pattern are compatible.
//    Return true, if number of replace insertions is not bigger, than
//    number of sub-matches in regular expression.
//
//    Equivalent of RE2::CheckRewriteString
fun check_rewrite_string(re: regex_t, rewrite: replace_pattern_t) : bool
{
    number_of_capturing_groups(regex_t {handle = re.find_r, find_r = re.find_r}) > max_submatch(rewrite)
}

/////////////////////////////////////////////////////////////////////////////////////////
// Main RE2 Functions ///////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

//    There is four main RE2 functions classes:
//    full_match       - Equivalent of RE2::FullMatch
//    partial_match    - Equivalent of RE2::PartialMatch
//    consume          - Equivalent of RE2::Consume
//    find_and_consume - Equivalent of RE2::FindAndConsume
//
//    full_match and partial_match returns only bool, designating success comparisson or search.
//    consume and find_and_consume also return position after successfully found match.
//
//    Functions with suffix "_n" also returns array of submatches in form of position pairs:
//    full_match_n, partial_match_n, consume_n, find_and_consume_n
//
//    Functions with suffix "_n_str" also returns array of submatches, as strings:
//    full_match_n_str, partial_match_n_str, consume_n_str, find_and_consume_n_str

//    Return true, if the whole given string corresponds to pattern.
//    E.g.,
//    full_match("Call the function without suffix.",Re2.compile(r".* ([[:alpha:]]+) suffix.")) = true
//    full_match("Call the function without suffix. But it would be nice to get submatch.",Re2.compile(r".*suffix.")) = false
//
//    Equivalent of RE2::FullMatch
@pure fun full_match(text: string, re: regex_t) : bool = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t ctext;
    int fx_status = fx_str2cstr(text, &ctext, 0, 0);
    if(fx_status>=0)
    {
        *fx_result = re2::RE2::FullMatch(ctext.data, *re_to_apply);
        fx_free_cstr(&ctext);
    }
    return fx_status;
}

//    Same as full_match, but, in case of successful match also returns array of sub-matches. Each sub-match is represented as
//    pair of positions: (first symbol of submatch, first symbol after submatch).
//
//    E.g.,
//    full_match_n("Call the function without suffix.",Re2.compile(r".* ([[:alpha:]]+) suffix.")) = (true, [(18, 25)])
//
//    Equivalent of RE2::FullMatchN
//                                                 (success, (sub_start, sub_end)[])
@pure fun full_match_n(text: string, re: regex_t) : (bool   , (int      , int    )[]) = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t ctext = {};
    int fx_status = fx_str2cstr(text, &ctext, 0, 0);
    if(fx_status>=0)
    {
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        try
        {
            std::vector<StringPiece> sub_string_pieces(sub_amount, StringPiece());
            std::vector<RE2::Arg*> sub_args(sub_amount, nullptr);
            std::vector<RE2::Arg> sub_args_storage(sub_amount);
            make_sub_args(sub_string_pieces, sub_args, sub_args_storage);
            fx_result->t0 = RE2::FullMatchN(ctext.data, *re_to_apply, &(*(sub_args.begin())), sub_amount);
            if(fx_result->t0)
            {
                fx_status = string_pieces_to_arr(sub_string_pieces, ctext.data, text, &(fx_result->t1));
            }
        }
        catch (std::bad_alloc& e)
        {
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
    }
fx_cleanup:
    fx_free_cstr(&ctext);
    return fx_status;
}

//    Same as full_match, but, in case of successful match also returns array of sub-matches, as strings.
//
//    E.g.,
//    full_match_n_str("Call the function without suffix.",Re2.compile(r".* ([[:alpha:]]+) suffix.")) = (true, ["without"])
//
//    Equivalent of RE2::FullMatchN
//                                                     (success, sub-matches [])
@pure fun full_match_n_str(text: string, re: regex_t) : (bool,         string [])
{
    val (success, starts_ends) = full_match_n(text, re)
    (success, incise(text, starts_ends))
}

//    Return true, if string contains part corresponding to pattern.
//    E.g.,
//    partial_match("Call the function without suffix.",Re2.compile(r"(without|with)")) = true
//    partial_match("Call the function to get false.",Re2.compile(r"(without|with)")) = false
//
//    Equivalent of RE2::PartialMatch
@pure fun partial_match(text: string, re: regex_t) : bool = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t ctext;
    int fx_status = fx_str2cstr(text, &ctext, 0, 0);
    if(fx_status>=0)
    {
        *fx_result = re2::RE2::PartialMatch(ctext.data, *re_to_apply);
        fx_free_cstr(&ctext);
    }
    return fx_status;
}

//    Same as partial_match, but, in case of successful search also returns array of sub-matches. Each sub-match is represented as
//    pair of positions: (first symbol of submatch, first symbol after submatch).
//
//    E.g.,
//    partial_match_n("Call the function with suffix.",Re2.compile(r"(without|with)")) = (true, [(18, 22)])
//
//    Equivalent of RE2::PartialMatchN
//                                                    (success, (sub_start, sub_end)[])
@pure fun partial_match_n(text: string, re: regex_t) : (bool   , (int      , int    )[]) = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t ctext = {};
    int fx_status = fx_str2cstr(text, &ctext, 0, 0);
    if(fx_status>=0)
    {
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        try
        {
            std::vector<StringPiece> sub_string_pieces(sub_amount, StringPiece());
            std::vector<RE2::Arg*> sub_args(sub_amount, nullptr);
            std::vector<RE2::Arg> sub_args_storage(sub_amount);
            make_sub_args(sub_string_pieces, sub_args, sub_args_storage);
            fx_result->t0 = RE2::PartialMatchN(ctext.data, *re_to_apply, &(*(sub_args.begin())), sub_amount);
            if(fx_result->t0)
            {
                fx_status = string_pieces_to_arr(sub_string_pieces, ctext.data, text, &(fx_result->t1));
            }
        }
        catch (std::bad_alloc& e)
        {
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
    }
fx_cleanup:
    fx_free_cstr(&ctext);
    return fx_status;
}

//    Same as partial_match,  but, in case of successful match also returns array of sub-matches, as strings.
//
//    E.g.,
//    partial_match_n_str("Call the function with suffix.",Re2.compile(r"(without|with)")) = (true, ["with"])
//
//    Equivalent of RE2::PartialMatchN
//                                                   (success, sub-matches [])
fun partial_match_n_str(text: string, re: regex_t) : (bool,         string [])
{
    val (success, starts_ends) = partial_match_n(text, re)
    (success, incise(text, starts_ends))
}

//    Return true, if substring, starting with pos position have prefix corresponding to pattern.
//    Also return position after pattern match. If comparisson wasn't successfull, returns pos.
//
//    E.g.,
//    consume("Call the function without suffix.", 0, Re2.compile(r"(without|with)")) = (false, 0)
//    consume("Call the function without suffix.", 18, Re2.compile(r"(without|with)")) = (true, 25)
//
//    Equivalent of RE2::Consume
//                                                  (success, newpos)
fun consume(input: string, pos: int, re: regex_t) : (bool   ,    int)
{
    val (success, newpos, arr) = consume_n(input, pos, re)
    (success, newpos)
}

//    Same as consume, but, in case of successful search also returns array of sub-matches. Each sub-match is represented as
//    pair of positions: (first symbol of submatch, first symbol after submatch).
//
//    E.g.,
//    consume_n("Call the function without suffix.", 9, Re2.compile(r"function (without|with) suffix")) = (true, 32,[(18,25)])
//
//    Equivalent of RE2::ConsumeN
//                                                         (success, newpos, (sub_start, sub_end)[])
@pure fun consume_n(input: string, pos: int, re: regex_t) : (bool   ,    int, (int      , int    )[]) = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t cinput = {};
    int fx_status = fx_str2cstr(input, &cinput, 0, 0);
    if(fx_status>=0)
    {
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        try
        {
            std::vector<StringPiece> sub_string_pieces(sub_amount, StringPiece());
            std::vector<RE2::Arg*> sub_args(sub_amount, nullptr);
            std::vector<RE2::Arg> sub_args_storage(sub_amount);
            make_sub_args(sub_string_pieces, sub_args, sub_args_storage);
            StringPiece string_n_position(cinput.data + pos);
            fx_result->t0 = RE2::ConsumeN(&string_n_position, *re_to_apply, &(*(sub_args.begin())), sub_amount);
            if(fx_result->t0)
            {
                fx_result->t1 = string_n_position.begin() - cinput.data;
                fx_status = string_pieces_to_arr(sub_string_pieces, cinput.data, input, &(fx_result->t2));
            }
            else
            {
                fx_result->t1 = pos;
            }
        }
        catch (std::bad_alloc& e)
        {
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
    }
fx_cleanup:
    fx_free_cstr(&cinput);
    return fx_status;
}

//    Same as consume, but, in case of successful match also returns array of sub-matches, as strings.
//
//    E.g.,
//    consume_n_str("Call the function without suffix.", 9, Re2.compile(r"function (without|with) suffix")) = (true, 32, ["without"])
//
//    Equivalent of RE2::ConsumeN
//                                                        (success, newpos, sub-matches [])
fun consume_n_str(input: string, pos: int, re: regex_t) : (bool   ,    int,      string [])
{
    val (success, newpos, starts_ends) = consume_n(input, pos, re)
    (success, newpos, incise(input, starts_ends))
}

//    Return true, if it's possible to find match to pattern in substring, starting with pos position.
//    Also return position after pattern match. If comparisson wasn't successfull, returns pos.
//
//    E.g.,
//    find_and_consume("Call the function without suffix.", 0, Re2.compile(r"(without|with)")) = (true, 25)
//
//    Equivalent of RE2::FindAndConsume
//                                                           (success, newpos)
fun find_and_consume(input: string, pos: int, re: regex_t) : (bool   ,    int)
{
    val (success, newpos, arr) = find_and_consume_n(input, pos, re)
    (success, newpos)
}

//    Same as find_and_consume, but, in case of successful search also returns array of sub-matches. Each sub-match is represented as
//    pair of positions: (first symbol of submatch, first symbol after submatch).
//
//    E.g.,
//    find_and_consume_n("Call the function without suffix. Is this string with without?", 0, Re2.compile(r"(without|with)")) = (true, 25,[(18,25)])
//    find_and_consume_n("Call the function without suffix. Is this string with without?", 25, Re2.compile(r"(without|with)")) = (true, 53,[(49,53)])
//    find_and_consume_n("Call the function without suffix. Is this string with without?", 53, Re2.compile(r"(without|with)")) = (true, 53,[(54,61)])
//
//    Equivalent of RE2::FindAndConsumeN
//                                                                  (success, newpos, (sub_start, sub_end)[])
@pure fun find_and_consume_n(input: string, pos: int, re: regex_t) : (bool   ,    int, (int      , int    )[]) = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t cinput = {};
    int fx_status = fx_str2cstr(input, &cinput, 0, 0);
    if(fx_status>=0)
    {
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        try
        {
            std::vector<StringPiece> sub_string_pieces(sub_amount, StringPiece());
            std::vector<RE2::Arg*> sub_args(sub_amount, nullptr);
            std::vector<RE2::Arg> sub_args_storage(sub_amount);
            make_sub_args(sub_string_pieces, sub_args, sub_args_storage);
            StringPiece string_n_position(cinput.data + pos);
            fx_result->t0 = RE2::FindAndConsumeN(&string_n_position, *re_to_apply, &(*(sub_args.begin())), sub_amount);
            if(fx_result->t0)
            {
                fx_result->t1 = string_n_position.begin() - cinput.data;
                fx_status = string_pieces_to_arr(sub_string_pieces, cinput.data, input, &(fx_result->t2));
            }
            else
            {
                fx_result->t1 = pos;
            }
        }
        catch (std::bad_alloc& e)
        {
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
    }
fx_cleanup:
    fx_free_cstr(&cinput);
    return fx_status;
}

//   Same as find_and_consume, but, in case of successful match also returns array of sub-matches, as strings.
//
//   E.g.,
//   find_and_consume_n("Call the function without suffix. Is this string with without?", 0, Re2.compile(r"(without|with)")) = (true, 25,["without"])
//   find_and_consume_n("Call the function without suffix. Is this string with without?", 25, Re2.compile(r"(without|with)")) = (true, 53,["with"])
//   find_and_consume_n("Call the function without suffix. Is this string with without?", 53, Re2.compile(r"(without|with)")) = (true, 53,["without"])
//
//   Equivalent of RE2::FindAndConsumeN
//                                                                 (success, newpos, sub-matches [])
fun find_and_consume_n_str(input: string, pos: int, re: regex_t) : (bool   ,    int,      string [])
{
    val (success, newpos, starts_ends) = find_and_consume_n(input, pos, re)
    (success, newpos, incise(input, starts_ends))
}

//    Flag type for configuring general_match function. Defines, would function's search be anchored to start of
//    given substing, to both of ends, or will be free.
//
//    Equivalent of RE2::Anchor
type anchor_t =
{
    anchor_start: bool = false;
    anchor_both : bool = false // [TODO]: In this case we need enum, but right now anchor_both just overrides anchor_start
}

//    Most flexible Re2 searching routine. Position range of search can be managed through startpos and
//    endpos arguments.
//    Search can be anchored to start(for matching prefix) or start and end(for matching whole range). Use
//    re_anchor argument for this.
//
//    Return success bool and array of sub-matches. Each sub-match is represented as
//    pair of positions: (first symbol of submatch, first symbol after submatch).
//
//    E.g.,
//    val strre = Re2.compile(r"^.*$",Re2.options_t {posix_syntax = true, one_line = false}) // Special options are needed to use ^ and $.
//    Re2.general_match("String 1\nString 2\nString 3\nString 4\n", strre, 6, 24, Re2.anchor_t {}) = (true, [(9, 17)])
//
//    Equivalent of RE2::Match (It's impossible to use "match" keyword as name of function in ficus)
//                                                                                             (success, (sub_start, sub_end)[])
fun general_match(text: string, re: regex_t, startpos: int, endpos: int, re_anchor: anchor_t): (bool   , (int      , int    )[]) = @ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr)
        return FX_EXN_NullPtrError;
    fx_cstr_t ctext = {};
    int fx_status = fx_str2cstr(text, &ctext, 0, 0);
    if(fx_status>=0)
    {
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        try
        {
            std::vector<StringPiece> sub_string_pieces(sub_amount, StringPiece());
            if(fx_status>=0)
            {
                RE2::Anchor anchr = re_anchor->anchor_both ? RE2::ANCHOR_BOTH : (re_anchor->anchor_start ? RE2::ANCHOR_START : RE2::UNANCHORED);
                fx_result->t0 = re_to_apply->Match(ctext.data, static_cast<size_t>(startpos), static_cast<size_t>(endpos), anchr, &(*(sub_string_pieces.begin())), sub_amount);
                if(fx_result->t0)
                {
                    fx_status = string_pieces_to_arr(sub_string_pieces, ctext.data, text, &(fx_result->t1));
                }
            }
        }
        catch (std::bad_alloc& e)
        {
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
    }
fx_cleanup:
    fx_free_cstr(&ctext);
    return fx_status;
}

//    Same, as general_match, but sub-mathes are returning as strings.
//
//    E.g.,
//    val strre = Re2.compile(r"^.*$",Re2.options_t {posix_syntax = true, one_line = false}) // Special options are needed to use ^ and $.
//    Re2.general_match("String 1\nString 2\nString 3\nString 4\n", strre, 6, 24, Re2.anchor_t {}) = (true, ["String 2"])
//
//    Equivalent of RE2::Match (It's impossible to use "match" keyword as name of function in ficus)
//                                                                                                 (success, sub-matches[])
fun general_match_str(text: string, re: regex_t, startpos: int, endpos: int, re_anchor: anchor_t): (bool   ,     string [])
{
    val (success, starts_ends) = general_match(text, re, startpos, endpos, re_anchor)
    (success, incise(text, starts_ends))
}


/////////////////////////////////////////////////////////////////////////////////////////
// Find /////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

//    C++ Re2 have no find or findall functions, but we need to declare before Re2-
//    like replace and global_replace, because they replace are not direct wrappers
//    around C++ functions, and they are dependent on find.
//
//    Search procedure. Returns true if pattern is found and array of sub-matches in form
//    of pair of positions: (first symbol of submatch, first symbol after submatch).
//    Zero-indexed pair is positions for whole match. That's the only difference to
//    partial_match_n
//
//    E.g.,
//    Re2.find("Some digital field, like 156454 or 456465. Ok?", r" (\d+) ") = (true, [(24, 32), (25, 31)])

//                                              (success, (sub_start, sub_end)[])
fun find(string_to_match: string, re: regex_t): (bool   , (int      , int    )[])
{
    partial_match_n(string_to_match, regex_t {handle = re.find_r, find_r = re.find_r})
}

//    Same, as find, but sub-mathes are returning as strings.
//
//    E.g.,
//    Re2.find("Some digital field, like 156454 or 456465. Ok?", r" (\d+) ") = (true, [" 156454 ", "156454"])

//                                                  (success, sub-matches[])
fun find_str(string_to_match: string, re: regex_t): (bool   , string     [])
{
    val (success, starts_ends) = find(string_to_match, re)
    (success, incise(string_to_match, starts_ends))
}

@ccode
{
    struct fx_re2_findall_process
    {
        std::vector<re2::StringPiece> sub_string_pieces;
        std::vector<re2::RE2::Arg*> sub_args;
        std::vector<re2::RE2::Arg> sub_args_storage;
        fx_cstr_t cloth;
        fx_str_t correction;
    };

    void fx_re2_findall_process_free(void* ptr)
    {
        if(ptr)
        {
            fx_re2_findall_process* proc = (fx_re2_findall_process*)(ptr);
            fx_free_cstr(&proc->cloth);
            fx_free_str(&proc->correction);
            delete proc;
        }
    }
}

//    Search procedure. Returns true if pattern is found and 2d-array of sub-matches of all matches
//    in form of pair of positions: (first symbol of submatch, first symbol after submatch).
//    Zero-indexed pair in each row is positions for whole match.
//
//    E.g.,
//    Re2.find("Some digital field, like 156454 or 456465. Ok?", r" (\d+).") = \
//    (true, [(24, 32), (25, 31);
//            (34, 42), (35, 41)])
//                                                 (success, (sub_start, sub_end)[,])
fun findall(string_to_match: string, re: regex_t): (bool   , (int      , int    )[,])
{
    @pure fun findall_init(input: string, re: regex_t) : cptr = @ccode
    {
        using namespace re2;
        RE2* re_to_apply = static_cast<RE2*>(re->find_r->ptr);
        if(re_to_apply == nullptr)
            return FX_EXN_NullPtrError;
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();
        fx_re2_findall_process* resptr = nullptr;
        int fx_status = FX_OK;
        try
        {
            resptr = new fx_re2_findall_process;
            resptr->cloth = {};
            resptr->correction = {};
            fx_copy_str(input, &(resptr->correction));
            resptr->sub_string_pieces.resize(sub_amount);
            resptr->sub_args.resize(sub_amount, nullptr);
            resptr->sub_args_storage.resize(sub_amount);
            make_sub_args(resptr->sub_string_pieces, resptr->sub_args, resptr->sub_args_storage);
        }
        catch (std::bad_alloc& e)
        {
            fx_re2_findall_process_free(resptr);
            FX_FAST_THROW(FX_EXN_OutOfMemError, fx_cleanup);
        }
        fx_status = fx_str2cstr(input, &resptr->cloth, 0, 0);
        if(fx_status < 0)
        {
            fx_re2_findall_process_free(resptr);
        }
        else
        {
            fx_status = fx_make_cptr(resptr, fx_re2_findall_process_free, fx_result);
        }
    fx_cleanup:
        return fx_status;
    }

    val proc = findall_init(string_to_match, re)
    @pure fun find_step(proc: cptr, pos: int, re: regex_t) : (bool, int, (int, int)[]) = @ccode
    {
        int fx_status = FX_OK;
        using namespace re2;
        RE2* re_to_apply = static_cast<RE2*>(re->find_r->ptr);
        if(re_to_apply == nullptr)
            return FX_EXN_NullPtrError;
        fx_re2_findall_process* p_proc = static_cast<fx_re2_findall_process*>(proc->ptr);
        StringPiece string_n_position(p_proc->cloth.data + pos);
        fx_result->t0 = RE2::FindAndConsumeN(&string_n_position, *re_to_apply, &(*(p_proc->sub_args.begin())), p_proc->sub_args.size());
        if(fx_result->t0)
        {
            fx_result->t1 = string_n_position.begin() - p_proc->cloth.data;
            fx_status = string_pieces_to_arr(p_proc->sub_string_pieces, p_proc->cloth.data, &(p_proc->correction), &(fx_result->t2));
        }
        else
        {
            fx_result->t1 = pos;
        }
        return fx_status;
    }

    fun result_as_list(pos: int, lastend: int, reslst: (int, int) [] list) : (int, int)[] list
    {
        val (success, newpos, matches) = find_step(proc, pos,re)
        if(success)
        {
            val (_,newend) = matches[0]
            val npos = if(newend == lastend) {newpos+1} else {newpos}
            val newlst = if (newend == lastend) { reslst } else { matches :: reslst }
            if(npos < string_to_match.length())
            {
                result_as_list(npos, newend, newlst)
            }
            else
            {
                List.rev(newlst)
            }
        }
        else
        {
            List.rev(reslst)
        }
    }
    val larrist = result_as_list(0, -1, [])
    val len_i = larrist.length()
    val len_j = if len_i!=0 { size(larrist.hd())} else { 0 }
    val matrix_res: (int,int) [,] = array((len_i, len_j), (0,0))
    var i = 0
    for l <- larrist
    {
        for j <- 0:len_j
        {
            matrix_res[i,j] = l[j]
        }
        i += 1
    }
    (len_i !=0, matrix_res)
}

//    Search procedure. Returns true if pattern is found and 2d-array of sub-matches of all matches
//    in form of pair of positions: (first symbol of submatch, first symbol after submatch).
//    Zero-indexed pair in each row is positions for whole match.
//
//    E.g.,
//    Re2.find("Some digital field, like 156454 or 456465. Ok?", r" (\d+).") = \
//    (true, [(24, 32), (25, 31);
//            (34, 42), (35, 41)])
//                                                     (success, sub-matches[,])
fun findall_str(string_to_match: string, re: regex_t): (bool   , string     [,])
{
    val (success, starts_ends) = findall(string_to_match, re)
    val (len_i, len_j) = size(starts_ends)
    (success, [| for i <- 0:len_i for j <- 0:len_j
                {
                    val(start,end) = starts_ends[i,j]
                    string_to_match[start:end]
                }
              |])
}

/////////////////////////////////////////////////////////////////////////////////////////
// Replaces /////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

//Service function
fun compose_replacement(cloth: string, french_curve: replace_pattern_t, found_subs: (int, int)[]) : string
{
    val fold rlist = [] for piece <- french_curve.pieces
        {
            match piece
            {
                | RPString simple_piece => simple_piece::rlist
                | RPInt sub_num =>
                    val (sub_start, sub_end) = found_subs[sub_num]
                    cloth[sub_start:sub_end]::rlist
            }
        }
    join("", List.rev(rlist))
}


//    Replace procedure. Search for first ocurrence of pattern-corresonding substrings and
//    replace it with string composed from rewrite pattern. Rewrite pattern can be compiled from
//    string with help of compile_replace_pattern. Original rewrite string can contain insertions,
//    designated like r"\d", where d is digit from 0 to 9, represents number of submatch in
//    found occurence. r"\0" is for whole match. Number of insertions cannot be bigger, than
//    number of sub-matches in regular expression. (Note, there are used r""-literals for avoiding
//    double slashes in replace pattern).
//
//    Returns true if pattern is found and string with replace made.
//
//    E.g.,
//    replace("Person: name@domain.com", r"(\w+)@(\w+)\.(\w+)", r"\1 from \2") = (true, "Person: name from domain")
//
//    Equivalent of RE2::Replace. *It's not a wrapper, replacing part is written on ficus.

//                                                                 (success, result)
fun replace(str: string, re: regex_t, rewrite: replace_pattern_t): (bool,    string)
{
    assert(check_rewrite_string(re, rewrite))
    val (is_found, ranges) = find(str, re)
    (is_found,
        if(is_found)
        {
            val (fullmatch_start, fullmatch_end) = ranges[0]
            val j_list = str[:fullmatch_start] :: compose_replacement(str, rewrite, ranges) ::str[fullmatch_end:] :: []
            String.join("", j_list)
        }
        else
        {str})
}

//    Replace all found occurencies of pattern-corresonding substrings with rewrite pattern.
//    See details of rewrite pattern in replace and compile_replace_pattern.
//
//    Returns true if pattern is found and string with replaces made.
//
//    E.g.,
//    replace("name1@domain1.com, name2@domain2.com, name3@domain3.com", r"(\w+)@(\w+)\.(\w+)", "\1 from \2") = (true, "name1 from domain1, name2 from domain2, name3 from domain3")
//
//    Equivalent of RE2::GlobalReplace. *It's not a wrapper, replacing part is written on ficus.

//                                                                        (success, result)
fun global_replace(str: string, re: regex_t, rewrite: replace_pattern_t): (bool,    string)
{
    assert(check_rewrite_string(re, rewrite))

    val (is_found, ranges) = findall(str, re)
    (is_found,
        if(is_found)
        {
            val (len_i, len_j) = size(ranges)
            var str_list = []
            var pos = 0
            for i <- 0: len_i
            {
                val (substart,subend) = ranges[i,0]
                str_list = compose_replacement(str, rewrite, ranges[i,:]) :: str[pos:substart] :: str_list
                pos = subend
            }
            str_list = str[pos:] :: str_list
            "".join("", str_list.rev())
        }
        else
        {str})
}

/////////////////////////////////////////////////////////////////////////////////////////
// Instant versions of functions. Strings used here instead of pre-compiled regexp object
// It's not recommended, when particular regexp will be used multiple times.
/////////////////////////////////////////////////////////////////////////////////////////

fun full_match(text: string, re: string) : bool
{
    full_match(text, compile(re))
}

//                                           (success, (sub_start, sub_end)[])
fun full_match_n(text: string, re: string) : (bool   , (int      , int    )[])
{
    full_match_n(text, compile(re))
}

//                                               (success, sub-matches [])
fun full_match_n_str(text: string, re: string) : (bool,         string [])
{
    full_match_n_str(text, compile(re))
}

fun partial_match(text: string, re: string) : bool
{
    partial_match(text, compile(re))
}

//                                              (success, (sub_start, sub_end)[])
fun partial_match_n(text: string, re: string) : (bool   , (int      , int    )[])
{
    partial_match_n(text, compile(re))
}

//                                                  (success, sub-matches [])
fun partial_match_n_str(text: string, re: string) : (bool,         string [])
{
    partial_match_n_str(text, compile(re))
}

//                                                 (success, newpos)
fun consume(input: string, pos: int, re: string) : (bool   ,    int)
{
    consume(input, pos, compile(re))
}

//                                                        (success, newpos, (sub_start, sub_end)[])
@pure fun consume_n(input: string, pos: int, re: string) : (bool   ,    int, (int      , int    )[])
{
    consume_n(input, pos, compile(re))
}

//                                                       (success, newpos, sub-matches [])
fun consume_n_str(input: string, pos: int, re: string) : (bool   ,    int,      string [])
{
    consume_n_str(input, pos, compile(re))
}

//                                                          (success, newpos)
fun find_and_consume(input: string, pos: int, re: string) : (bool   ,    int)
{
    find_and_consume(input, pos, compile(re))
}

//                                                                 (success, newpos, (sub_start, sub_end)[])
@pure fun find_and_consume_n(input: string, pos: int, re: string) : (bool   ,    int, (int      , int    )[])
{
    find_and_consume_n(input, pos, compile(re))
}

//                                                                (success, newpos, sub-matches [])
fun find_and_consume_n_str(input: string, pos: int, re: string) : (bool   ,    int,      string [])
{
    find_and_consume_n_str(input, pos, compile(re))
}

//                                                                                            (success, (sub_start, sub_end)[])
fun general_match(text: string, re: string, startpos: int, endpos: int, re_anchor: anchor_t): (bool   , (int      , int    )[])
{
    general_match(text, compile(re), startpos, endpos, re_anchor)
}

//                                                                                                (success, sub-matches[])
fun general_match_str(text: string, re: string, startpos: int, endpos: int, re_anchor: anchor_t): (bool   ,     string [])
{
    general_match_str(text, compile(re), startpos, endpos, re_anchor)
}

//                                             (success, (sub_start, sub_end)[])
fun find(string_to_match: string, re: string): (bool   , (int      , int    )[])
{
    find(string_to_match, compile(re))
}

//                                                 (success, sub-matches[])
fun find_str(string_to_match: string, re: string): (bool   , string     [])
{
    find_str(string_to_match, compile(re))
}

//                                                (success, (sub_start, sub_end)[,])
fun findall(string_to_match: string, re: string): (bool   , (int      , int    )[,])
{
    findall(string_to_match, compile(re))
}

//                                                    (success, sub-matches[,])
fun findall_str(string_to_match: string, re: string): (bool   , string     [,])
{
    findall_str(string_to_match, compile(re))
}

//                                                                (success, result)
fun replace(str: string, re: string, rewrite: replace_pattern_t): (bool,    string)
{
    replace(str, compile(re), rewrite)
}

//                                                           (success, result)
fun replace(str: string, re: regex_t, rewrite: string): (bool,    string)
{
    replace(str, re, compile_replace_pattern(rewrite))
}

//                                                     (success, result)
fun replace(str: string, re: string, rewrite: string): (bool,    string)
{
    replace(str, compile(re), rewrite)
}

//                                                                       (success, result)
fun global_replace(str: string, re: string, rewrite: replace_pattern_t): (bool,    string)
{
    global_replace(str, compile(re), rewrite)
}

//                                                                  (success, result)
fun global_replace(str: string, re: regex_t, rewrite: string): (bool,    string)
{
    global_replace(str, re, compile_replace_pattern(rewrite))
}

//                                                            (success, result)
fun global_replace(str: string, re: string, rewrite: string): (bool,    string)
{
    global_replace(str, compile(re), rewrite)
}
