/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Re2 library wrapper.

    Usage: 
    1.) Re2 library must be installed in system, see installation instructions there:
    https://github.com/google/re2/wiki/Install

    Sometimes(e.g., on Debian 10) it's also needed to run
        sudo ldconfig 
    after installation for creating appropriate soft links for shared libraries.

    2.) Before compilation set environment variable FICUS_LINK_LIBRARIES to "-lre2" for providing compiler info about dependencies.
        e.g, in bash:
        export FICUS_LINK_LIBRARIES="-lre2"
    3.) Use -c++ flag on compilation.
*/

exception BadRegexp : string
ccode 
{
    #include <re2/re2.h>
    #include <vector>

    int _fx_M3Re2FM14make_BadRegexpE1S(fx_str_t* arg0, fx_exn_t* fx_result); //[TODO] : Exception, declared before ccode block must be declared in program, translated to C program before this ccode for avoiding such insets. 

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
        fx_free_exn(&v_0);
        return fx_status;
    }

    #define FX_RE2_THROW_BADREGEXP(str, catch_label) { fx_status = throwRe2Badregexp((str)); goto catch_label; }

    void fx_re2_free(void* ptr)
    {
        using namespace re2;
        delete static_cast<RE2*>(ptr);
    }

    static void free_sub_args(std::vector<re2::RE2::Arg*>& a_sub_args)
    {
        for(size_t arg_num = 0; arg_num < a_sub_args.size(); arg_num++)
        {
            delete a_sub_args[arg_num];
        }
    }

    static int make_sub_args(std::vector<re2::StringPiece>& a_sub_string_pieces, std::vector<re2::RE2::Arg*>& a_sub_args)
    {
        using namespace re2;
        for(size_t arg_num = 0; arg_num < a_sub_args.size(); arg_num++)
        {
            try
            {
                a_sub_args[arg_num] = new RE2::Arg(&a_sub_string_pieces[arg_num]);
            }
            catch (std::bad_alloc& e)
            {
                free_sub_args(a_sub_args);
                return FX_EXN_OutOfMemError;
            }
        }
        return FX_OK;
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
    [for (start, end) <- starts_ends {str[start:end]}]
}

/////////////////////////////////////////////////////////////////////////////////////////
// regex_t - precompiled regexp pattern.                                             ////
// replace_pattern_t describes, what to replace found matches                        ////
/////////////////////////////////////////////////////////////////////////////////////////

type regex_t = { handle: cptr, find_r: cptr}
type replace_piece_t =
| RPInt: int
| RPString: string

type options_t = 
{
    posix_syntax: bool = false,
    longest_match: bool = false,
//    max_mem: int = kDefaultMaxMem,
    literal: bool = false,
    never_nl: bool = false,
    dot_nl: bool = false,
    never_capture: bool = false,
    case_sensitive: bool = true,
    perl_classes: bool = false,
    word_boundary: bool = false,
    one_line: bool = false
}

type replace_pattern_t = {pieces: replace_piece_t list, max_subnum: int}

fun compile(pattern: string, options: options_t): regex_t
{
    fun compile_re2(pattern: string, options: options_t): cptr = ccode
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

val digit_reg = compile("\\\\[0-9]")

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
                val placeholder_num = getOpt(atoi(rewrite[substart+1:substart+2]), 0)
                pieces_list = RPInt(placeholder_num) :: RPString(rewrite[pos:substart]) :: pieces_list
                pos = subend
            }
            pieces_list = RPString(rewrite[pos:]) :: pieces_list
            fold filtered = [] for piece <- pieces_list
            {   
                val ok = match piece { | RPString simple_piece => String.length(simple_piece)!=0  | RPInt sub_num => true}
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

pure fun string(re: regex_t): string = ccode  
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr) 
        return FX_EXN_NullPtrError;
    return fx_cstr2str(re_to_apply->pattern().c_str(), re_to_apply->pattern().size(), fx_result);
}

pure fun number_of_capturing_groups(re: regex_t): int = ccode
{
    using namespace re2;
    RE2* re_to_apply = static_cast<RE2*>(re->handle->ptr);
    if(re_to_apply == nullptr) 
        return FX_EXN_NullPtrError;
    *fx_result = re_to_apply->NumberOfCapturingGroups();
    return FX_OK;
}

fun named_capturing_groups(re: regex_t): (string, int) list
{
    pure fun named_capturing_groups_(re: regex_t): (string [], int []) = ccode
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

fun capturing_group_names(re: regex_t): (int, string) list
{
    val lst = named_capturing_groups(re)
    [: for (name, index) <- lst {(index, name)} :]
}

fun max_submatch(rewrite: replace_pattern_t): int
{
    rewrite.max_subnum
}

fun check_rewrite_string(re: regex_t, rewrite: replace_pattern_t) : bool 
{
    number_of_capturing_groups(regex_t {handle = re.find_r, find_r = re.find_r}) > max_submatch(rewrite)
}

pure fun full_match(text: string, re: regex_t) : bool = ccode
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

//                                                 (success, (sub_start, sub_end)[])
pure fun full_match_n(text: string, re: regex_t) : (bool   , (int      , int    )[]) = ccode
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
            fx_status = make_sub_args(sub_string_pieces, sub_args);
            if(fx_status>=0)
            {
                fx_result->t0 = RE2::FullMatchN(ctext.data, *re_to_apply, &(*(sub_args.begin())), sub_amount);
                if(fx_result->t0)
                {
                    fx_status = string_pieces_to_arr(sub_string_pieces, ctext.data, text, &(fx_result->t1));
                }
                free_sub_args(sub_args);
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

//                                                     (success, sub-matches [])
pure fun full_match_n_str(text: string, re: regex_t) : (bool,         string [])
{   
    val (success, starts_ends) = full_match_n(text, re)
    (success, incise(text, starts_ends))
}

pure fun partial_match(text: string, re: regex_t) : bool = ccode
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

//                                                    (success, (sub_start, sub_end)[])
pure fun partial_match_n(text: string, re: regex_t) : (bool   , (int      , int    )[]) = ccode
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
            fx_status = make_sub_args(sub_string_pieces, sub_args);
            if(fx_status>=0)
            {
                fx_result->t0 = RE2::PartialMatchN(ctext.data, *re_to_apply, &(*(sub_args.begin())), sub_amount);
                if(fx_result->t0)
                {
                    fx_status = string_pieces_to_arr(sub_string_pieces, ctext.data, text, &(fx_result->t1));
                }
                free_sub_args(sub_args);
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

//                                                   (success, sub-matches [])
fun partial_match_n_str(text: string, re: regex_t) : (bool,         string [])
{   
    val (success, starts_ends) = partial_match_n(text, re)
    (success, incise(text, starts_ends))
}

//                                                  (success, newpos)
fun consume(input: string, pos: int, re: regex_t) : (bool   ,    int)
{   
    val (success, newpos, arr) = consume_n(input, pos, re)
    (success, newpos)
}

//                                                         (success, newpos, (sub_start, sub_end)[])
pure fun consume_n(input: string, pos: int, re: regex_t) : (bool   ,    int, (int      , int    )[]) = ccode
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
            fx_status = make_sub_args(sub_string_pieces, sub_args);
            if(fx_status>=0)
            {
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
                free_sub_args(sub_args);
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

//                                                        (success, newpos, sub-matches [])
fun consume_n_str(input: string, pos: int, re: regex_t) : (bool   ,    int,      string [])
{   
    val (success, newpos, starts_ends) = consume_n(input, pos, re)
    (success, newpos, incise(input, starts_ends))
}

//                                                           (success, newpos)
fun find_and_consume(input: string, pos: int, re: regex_t) : (bool   ,    int)
{   
    val (success, newpos, arr) = find_and_consume_n(input, pos, re)
    (success, newpos)
}

//                                                                  (success, newpos, (sub_start, sub_end)[])
pure fun find_and_consume_n(input: string, pos: int, re: regex_t) : (bool   ,    int, (int      , int    )[]) = ccode
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
            fx_status = make_sub_args(sub_string_pieces, sub_args);
            if(fx_status>=0)
            {
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
                free_sub_args(sub_args);
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

//                                                                 (success, newpos, sub-matches [])
fun find_and_consume_n_str(input: string, pos: int, re: regex_t) : (bool   ,    int,      string [])
{   
    val (success, newpos, starts_ends) = find_and_consume_n(input, pos, re)
    (success, newpos, incise(input, starts_ends))
}

type anchor_t = 
{
    anchor_start: bool = false,
    anchor_both : bool = false // [TODO]: In this case we need enum, but right now anchor_both just overrides anchor_start
}

//This function is equivalent of RE2::Match
//                                                                                             (success, (sub_start, sub_end)[])
fun general_match(text: string, re: regex_t, startpos: int, endpos: int, re_anchor: anchor_t): (bool   , (int      , int    )[]) = ccode
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

//                                                                                                 (success, sub-matches[])
fun general_match_str(text: string, re: regex_t, startpos: int, endpos: int, re_anchor: anchor_t): (bool   ,     string [])
{
    val (success, starts_ends) = general_match(text, re, startpos, endpos, re_anchor)
    (success, incise(text, starts_ends))
}


/////////////////////////////////////////////////////////////////////////////////////////
// Find /////////////////////////////////////////////////////////////////////////////////
// C++ Re2 have no find or findall functions, but we need to declare before Re2-    /////
// like replace and global_replace, because they are dependent.                     /////
// find is wrapper of partial_match_n, but adds whole match at zero position in     /////
// result array.                                                                    /////
/////////////////////////////////////////////////////////////////////////////////////////

//                                              (success, (sub_start, sub_end)[])
fun find(string_to_match: string, re: regex_t): (bool   , (int      , int    )[])
{
    partial_match_n(string_to_match, regex_t {handle = re.find_r, find_r = re.find_r})
}

//                                                  (success, sub-matches[])
fun find_str(string_to_match: string, re: regex_t): (bool   , string     [])
{   
    val (success, starts_ends) = find(string_to_match, re)
    (success, incise(string_to_match, starts_ends))
}

ccode
{
    struct fx_re2_findall_process
    {
        std::vector<re2::StringPiece> sub_string_pieces;
        std::vector<re2::RE2::Arg*> sub_args;
        fx_cstr_t cloth;
        fx_str_t correction;
    };

    void fx_re2_findall_process_free(void* ptr)
    {
        if(ptr) 
        {
            fx_re2_findall_process* proc = (fx_re2_findall_process*)(ptr);
            free_sub_args(proc->sub_args);
            fx_free_cstr(&proc->cloth);
            fx_free_str(&proc->correction);
            delete proc;
        }
    }
}

//                                                 (success, (sub_start, sub_end)[,])
fun findall(string_to_match: string, re: regex_t): (bool   , (int      , int    )[,])
{
    pure fun findall_init(input: string, re: regex_t) : cptr = ccode
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
            fx_status = make_sub_args(resptr->sub_string_pieces, resptr->sub_args);
            if(fx_status < 0)
                throw std::bad_alloc();
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
            if(fx_status < 0)
            {
                fx_re2_findall_process_free(resptr);
            }
        }
    fx_cleanup:
        return fx_status;
    }

    val proc = findall_init(string_to_match, re)
    pure fun find_step(proc: cptr, pos: int, re: regex_t) : (bool, int, (int, int)[]) = ccode
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
            if(npos < String.length(string_to_match)) 
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
    val len_i = List.length(larrist)
    val len_j = if len_i!=0 { size(List.hd(larrist))} else { 0 }
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

//                                                     (success, sub-matches[,])
fun findall_str(string_to_match: string, re: regex_t): (bool   , string     [,])
{   
    val (success, starts_ends) = findall(string_to_match, re)
    val (len_i, len_j) = size(starts_ends)
    (success, [ for i <- 0:len_i for j <- 0:len_j 
                {
                    val(start,end) = starts_ends[i,j]
                    string_to_match[start:end]
                }
              ])
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
            String.join("", List.rev(str_list))
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
pure fun consume_n(input: string, pos: int, re: string) : (bool   ,    int, (int      , int    )[])
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
pure fun find_and_consume_n(input: string, pos: int, re: string) : (bool   ,    int, (int      , int    )[])
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
