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

ccode 
{
    #include <re2/re2.h>
    #include <vector>

    void fx_re2_free(void* ptr)
    {
        using namespace re2;
        RE2* re_to_delete = static_cast<RE2*>(ptr);
        re_to_delete->~RE2();
        fx_free(re_to_delete);
    }

    //TODO: Do something with these sophisticated constructions around exceptions.
    #define FX_RE2_THROW_RET(exn) return FX_SET_EXN(&_fx_exn_re2_##exn, false);
    #define FX_RE2_DECLARE_EXN(exn) \
        static int FX_EXN_RE2_##exn = 0; \
        static fx_exn_info_t FX_EXN_RE2_##exn##_info = {}; \
        static fx_exn_t _fx_exn_re2_##exn = {};

    #define FX_RE2_REGISTER_EXN(exn) fx_register_simple_exn(U"Re2."#exn, &FX_EXN_RE2_##exn, &FX_EXN_RE2_##exn##_info, &_fx_exn_re2_##exn)
    static re2::RE2::Options fx_re2_silent_opts;

    FX_RE2_DECLARE_EXN(BadRegexp);

    class FX_RE2_Initializer
    {
    public:

        FX_RE2_Initializer()
        {
            FX_RE2_REGISTER_EXN(BadRegexp);
            fx_re2_silent_opts.set_log_errors(false);
        }
    };
    static FX_RE2_Initializer fx_re2_initializer;

    static void set_sub_args(std::vector<re2::StringPiece>& a_sub_string_pieces, std::vector<re2::RE2::Arg*>& a_sub_args)
    {
        a_sub_args.resize(a_sub_string_pieces.size());
        for(size_t arg_num = 0; arg_num < a_sub_string_pieces.size(); arg_num++)
        {
            a_sub_args[arg_num] = new re2::RE2::Arg(&(a_sub_string_pieces[arg_num]));
        }
    }

    static void free_sub_args(const std::vector<RE2::Arg*>& a_sub_args)
    {
        for(size_t arg_num = 0; arg_num < a_sub_args.size(); arg_num++)
        {
            delete a_sub_args[arg_num];
        }
    }

    int string_pieces_to_arr(std::vector<re2::StringPiece>& sub_string_pieces, const char* string_itself, fx_arr_t* fx_result)
    {
        const int_ sub_amount = sub_string_pieces.size();
        const int_ arrdims[1] = {sub_amount};
        std::vector<int_> starts_and_ends(sub_amount * 2); //TODO: what if it's not enough memory???
        for(int piece_num = 0; piece_num < sub_amount; piece_num++)
        {
            starts_and_ends[2 * piece_num    ] = sub_string_pieces[piece_num].begin() - string_itself;
            starts_and_ends[2 * piece_num + 1] = sub_string_pieces[piece_num].end()   - string_itself;
        }
        return fx_make_arr( 1, arrdims, sizeof(int_)+sizeof(int_), 0, 0, &(*(starts_and_ends.begin())), fx_result);
    }
}

fun incise(string_to_match: string, start_end: (int, int)): string
{
    val (start, end) = start_end
    String.substr(string_to_match,start, end - start)
}

fun incise(string_to_match: string, starts_ends: (int, int)[]): string []
{
    [for start_end <- starts_ends {incise(string_to_match, start_end)}]
}

/////////////////////////////////////////////////////////////////////////////////////////
// Regexp pre-compiled objects for different purposes and compilation funcions for   ////
// them.                                                                             ////
// regex_t - standart regexp object for full_match, partial_match, consume,          ////
// find_and_consume functions.                                                       ////
// find_regex_t is used for find and replace functions.                              ////
// replace_pattern_t describes, what to replace found matches                        ////
/////////////////////////////////////////////////////////////////////////////////////////

//[TODO]  Synchronyze function and argument naming with original Re2.
//[TODO]  Named groups
//[TODO]  Check all other functions
type regex_t = { handle: cptr }
type find_regex_t = { handle: cptr }
type replace_piece_t =
| RPInt: int
| RPString: string

//                       (pieces              , max_subnum) [TODO]: Replace it with structure, when you will understand how to do this
type replace_pattern_t = (replace_piece_t list,        int)

fun compile(rstr: string/*TODO:Options*/): regex_t = ccode
{
    using namespace re2;
    fx_cstr_t crstr;
    int fx_status = fx_str2cstr(rstr, &crstr, 0, 0);
    if(fx_status>=0)
    {
        RE2* new_re = static_cast<RE2*>(fx_malloc(sizeof(re2::RE2)));
        new (new_re) RE2(crstr.data, fx_re2_silent_opts); //TODO: After construction we have to ask this object if it's created. Ok function will help with it. If no, we have to throw some exception.
        fx_free_cstr(&crstr);
        fx_status = fx_make_cptr(new_re, fx_re2_free, &fx_result->handle);
        if(!new_re->ok())
            FX_RE2_THROW_RET(BadRegexp);
    }
    return fx_status;
}

fun compile_for_find(rstr: string/*TODO:Options*/): find_regex_t
{
    pure fun usual_to_find(from_who: regex_t): find_regex_t = ccode //[TODO] Both find_to_usual and usual_to_find, kind of stupid. It's better to avoid need in them.
    {
        fx_copy_cptr(from_who->handle, &fx_result->handle);
        return FX_OK;
    }
    usual_to_find(compile("("+rstr+")")) 
}

pure fun number_of_capturing_groups(regexp: regex_t): int = ccode
{
    RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr); //[TODO]: Check if it's exists???
    *fx_result = re_to_apply->NumberOfCapturingGroups();
    return FX_OK;
}

pure fun full_match(string_to_match: string, regexp: regex_t) : bool = ccode
{   
    using namespace re2;
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        *fx_result = re2::RE2::FullMatch(cstring_to_match.data, *re_to_apply);//TODO: Try - catch.
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

//                                                                (success, (sub_start, sub_end)[])
pure fun full_match_n(string_to_match: string, regexp: regex_t) : (bool   , (int      , int    )[]) = ccode
{   
    using namespace re2;
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();

        std::vector<StringPiece> sub_string_pieces(sub_amount);
        std::vector<RE2::Arg*> sub_args;
        set_sub_args(sub_string_pieces, sub_args);

        fx_result->t0 = RE2::FullMatchN(cstring_to_match.data, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!
        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            fx_status = string_pieces_to_arr(sub_string_pieces, cstring_to_match.data, &(fx_result->t1));
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

pure fun full_match_n_str(string_to_match: string, regexp: regex_t) : (bool, string [])
{   
    val (success, starts_ends) = full_match_n(string_to_match, regexp)
    (success, incise(string_to_match, starts_ends))
}

pure fun partial_match(string_to_match: string, regexp: regex_t) : bool = ccode
{
    using namespace re2;
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        *fx_result = re2::RE2::PartialMatch(cstring_to_match.data, *re_to_apply);//TODO: Try - catch.
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

//                                                                   (success, (sub_start, sub_end)[])
pure fun partial_match_n(string_to_match: string, regexp: regex_t) : (bool   , (int      , int    )[]) = ccode
{   
    using namespace re2;
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();

        std::vector<StringPiece> sub_string_pieces(sub_amount);
        std::vector<RE2::Arg*> sub_args;
        set_sub_args(sub_string_pieces, sub_args);

        fx_result->t0 = RE2::PartialMatchN(cstring_to_match.data, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!
        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            fx_status = string_pieces_to_arr(sub_string_pieces, cstring_to_match.data, &(fx_result->t1));
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

fun partial_match_n_str(string_to_match: string, regexp: regex_t) : (bool, string [])
{   
    val (success, starts_ends) = partial_match_n(string_to_match, regexp)
    (success, incise(string_to_match, starts_ends))
}

//                                                                (success, newpos)
fun consume(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int)
{   
    val (success, newpos, arr) = consume_n(string_to_match, pos, regexp)
    (success, newpos)
}

//                                                                       (success, newpos, (sub_start, sub_end)[])
pure fun consume_n(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int, (int      , int    )[]) = ccode
{   
    using namespace re2;
    if(pos<0 || string_to_match->length < pos) 
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();

        std::vector<StringPiece> sub_string_pieces(sub_amount);
        std::vector<RE2::Arg*> sub_args;
        set_sub_args(sub_string_pieces, sub_args);

        StringPiece string_n_position(cstring_to_match.data + pos);
        fx_result->t0 = RE2::ConsumeN(&string_n_position, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!

        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            fx_result->t1 = string_n_position.begin() - cstring_to_match.data;
            fx_status = string_pieces_to_arr(sub_string_pieces, cstring_to_match.data, &(fx_result->t2));
        }
        else
        {
            fx_result->t1 = pos;
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

//                                                                      (success, newpos, sub-matches [])
fun consume_n_str(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int,      string [])
{   
    val (success, newpos, starts_ends) = consume_n(string_to_match, pos, regexp)
    (success, newpos, incise(string_to_match, starts_ends))
}

//                                                                         (success, newpos)
fun find_and_consume(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int)
{   
    val (success, newpos, arr) = find_and_consume_n(string_to_match, pos, regexp)
    (success, newpos)
}

//                                                                                (success, newpos, (sub_start, sub_end)[])
pure fun find_and_consume_n(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int, (int      , int    )[]) = ccode
{   
    using namespace re2;
    if(pos<0 || string_to_match->length < pos) 
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    fx_cstr_t cstring_to_match;
    int fx_status = fx_str2cstr(string_to_match, &cstring_to_match, 0, 0);
    if(fx_status>=0)
    {
        RE2* re_to_apply = static_cast<RE2*>(regexp->handle->ptr);
        const int sub_amount = re_to_apply->NumberOfCapturingGroups();

        std::vector<StringPiece> sub_string_pieces(sub_amount);
        std::vector<RE2::Arg*> sub_args;
        set_sub_args(sub_string_pieces, sub_args);

        StringPiece string_n_position(cstring_to_match.data + pos);
        fx_result->t0 = RE2::FindAndConsumeN(&string_n_position, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!

        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            fx_result->t1 = string_n_position.begin() - cstring_to_match.data;
            fx_status = string_pieces_to_arr(sub_string_pieces, cstring_to_match.data, &(fx_result->t2));
        }
        else
        {
            fx_result->t1 = pos;
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

//                                                                               (success, newpos, sub-matches [])
fun find_and_consume_n_str(string_to_match: string, pos: int, regexp: regex_t) : (bool   ,    int,      string [])
{   
    val (success, newpos, starts_ends) = find_and_consume_n(string_to_match, pos, regexp)
    (success, newpos, incise(string_to_match, starts_ends))
}

/////////////////////////////////////////////////////////////////////////////////////////
// Find /////////////////////////////////////////////////////////////////////////////////
// C++ Re2 have no find or findall functions, but we need to declare before Re2-    /////
// like replace and global_replace, because they are dependent.                     /////
// find is wrapper of partial_match_n, but adds whole match at zero position in     /////
// result array.                                                                    /////
/////////////////////////////////////////////////////////////////////////////////////////

//                                                       (success, (sub_start, sub_end)[])
fun find(string_to_match: string, regexp: find_regex_t): (bool   , (int      , int    )[])
{
    pure fun find_to_usual(from_who: find_regex_t): regex_t = ccode //[TODO] Both find_to_usual and usual_to_find, kind of stupid. It's better to avoid need in them.
    {
        fx_copy_cptr(from_who->handle, &fx_result->handle);
        return FX_OK;
    }
    partial_match_n(string_to_match, find_to_usual(regexp))
}

//                                                           (success, sub-matches[])
fun find_str(string_to_match: string, regexp: find_regex_t): (bool   , string     [])
{   
    val (success, starts_ends) = find(string_to_match, regexp)
    (success, incise(string_to_match, starts_ends))
}

//                                                          (success, (sub_start, sub_end)[,])
fun findall(string_to_match: string, regexp: find_regex_t): (bool   , (int      , int    )[,])
{
    pure fun find_to_usual(from_who: find_regex_t): regex_t = ccode //[TODO] Both find_to_usual and usual_to_find, kind of stupid. It's better to avoid need in them.
    {
        fx_copy_cptr(from_who->handle, &fx_result->handle);
        return FX_OK;
    }
    val usual = find_to_usual(regexp)
    fun result_as_list(pos: int) : (int, int)[] list
    {
        val (success, newpos, matches) = find_and_consume_n(string_to_match, pos, usual)
        if(success)
        {
            matches::result_as_list(newpos)
        }
        else
        {
            []
        }
    }
    val larrist = result_as_list(0)
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

//                                                              (success, sub-matches[,])
fun findall_str(string_to_match: string, regexp: find_regex_t): (bool   , string     [,])
{   
    val (success, starts_ends) = findall(string_to_match, regexp)
    val (len_i, len_j) = size(starts_ends)
    (success, [for i <- 0:len_i for j <- 0:len_j {incise(string_to_match, starts_ends[i,j])}])
}

/////////////////////////////////////////////////////////////////////////////////////////
// Replaces /////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

val digit_reg = compile("(\\\\[0-9])") 
fun compile_replace_pattern(rewrite: string): replace_pattern_t //[TODO] Ugly implementation. Do something with it. Add tail recursion, e.t.c...
{
    fun compile_replace_pattern_(rewrite: string, pos: int): replace_piece_t list
    {
        val (digit_is_found, newpos, found_range) = find_and_consume_n(rewrite, pos, digit_reg)
        if digit_is_found
        {
            val (startpos, _) = found_range[0]
            val justzero = "0"//[TODO] Remove this super ugly atoi.
            val placeholder_num = int(rewrite[startpos+1]) - int(justzero[0])
            if(startpos > pos) 
            {
                RPString(String.substr(rewrite, pos, startpos-pos))::RPInt(placeholder_num)::compile_replace_pattern_(rewrite, newpos)
            }
            else
            {
                RPInt(placeholder_num)::compile_replace_pattern_(rewrite, newpos)
            }
        }
        else
        {
            if(pos < String.length(rewrite))
            {
                RPString(String.substr(rewrite, pos, String.length(rewrite)-pos))::[]
            }
            else
            {
                []
            }
        }
    }
    val piece_lst = compile_replace_pattern_(rewrite,0)
    val fold max_sub = -1 for piece <- piece_lst
        {   
            match piece
            {
                | RPString simple_piece => max_sub
                | RPInt sub_num => max(max_sub, sub_num) 
            }
        }    

    (piece_lst, max_sub)
}

//Service function
fun compose_replacement(cloth: string, french_curve: replace_pattern_t, found_subs: (int, int)[]) : string 
{
    val (frcurve, _) = french_curve
    val fold rlist = [] for piece <- frcurve 
        {   
            match piece
            {
                | RPString simple_piece => simple_piece::rlist
                | RPInt sub_num => 
                    val (sub_start, sub_end) = found_subs[sub_num]
                    String.substr(cloth, sub_start, sub_end - sub_start)::rlist
            }
        }
    join("", List.rev(rlist))
}


//                                                                          (success, result)
fun replace(str: string, regexp: find_regex_t, rewrite: replace_pattern_t): (bool,    string)
{
    pure fun find_to_usual(from_who: find_regex_t): regex_t = ccode //[TODO] Both find_to_usual and usual_to_find, kind of stupid. It's better to avoid need in them.
    {
        fx_copy_cptr(from_who->handle, &fx_result->handle);
        return FX_OK;
    }
    val (_, max_sub_num) = rewrite
    if number_of_capturing_groups(find_to_usual(regexp)) <= max_sub_num
        {throw BadArgError} //[TODO]: BadArg or something more detailed? 
    val (is_found, ranges) = find(str, regexp)
    (is_found,
        if(is_found)
        {
            val (fullmatch_start, fullmatch_end) = ranges[0]
            val j_list = String.substr(str, 0, fullmatch_start) :: compose_replacement(str, rewrite, ranges) :: String.substr(str, fullmatch_end, String.length(str) - fullmatch_end) :: []
            String.join("", j_list)
        }
        else
        {str})
}

//                                                                                 (success, result)
fun global_replace(str: string, regexp: find_regex_t, rewrite: replace_pattern_t): (bool,    string)
{
    pure fun find_to_usual(from_who: find_regex_t): regex_t = ccode //[TODO] Both find_to_usual and usual_to_find, kind of stupid. It's better to avoid need in them.
    {
        fx_copy_cptr(from_who->handle, &fx_result->handle);
        return FX_OK;
    }
    val (_, max_sub_num) = rewrite
    if number_of_capturing_groups(find_to_usual(regexp)) <= max_sub_num
        {throw BadArgError} //[TODO]: BadArg or something more detailed? 

    val (is_found, ranges) = findall(str, regexp)
    (is_found,
        if(is_found)
        {
            val (len_i, len_j) = size(ranges) //[TODO]: Attempt to call size on matrix causes C compile error.
            //println(len_i)
            //""
            var str_list = []
            var pos = 0
            for i <- 0: len_i
            {   
                val (substart,subend) = ranges[i,0]
                str_list = compose_replacement(str, rewrite, ranges[i,:]) :: String.substr(str, pos, substart - pos) :: str_list
                pos = subend
            }
            str_list = String.substr(str, pos, String.length(str) - pos) :: str_list
            String.join("", List.rev(str_list))
        }
        else
        {str})
}

/////////////////////////////////////////////////////////////////////////////////////////
// Instant versions of functions. Strings used here instead of pre-compiled regexp object
// It's not recommended, when particular regexp will be used multiple times.
/////////////////////////////////////////////////////////////////////////////////////////

fun full_match(string_to_match: string, regexp: string) : bool
{   
    full_match(string_to_match, compile(regexp)) 
}

//                                                          (success, (sub_start, sub_end)[])
fun full_match_n(string_to_match: string, regexp: string) : (bool   , (int      , int    )[])
{
    full_match_n(string_to_match, compile(regexp)) 
}

fun full_match_n_str(string_to_match: string, regexp: string) : (bool, string [])
{
    full_match_n_str(string_to_match, compile(regexp))
}

fun partial_match(string_to_match: string, regexp: string) : bool
{
    partial_match(string_to_match, compile(regexp))
}

//                                                             (success, (sub_start, sub_end)[])
fun partial_match_n(string_to_match: string, regexp: string) : (bool   , (int      , int    )[])
{   
    partial_match_n(string_to_match, compile(regexp))
}

fun partial_match_n_str(string_to_match: string, regexp: string) : (bool, string [])
{   
    partial_match_n_str(string_to_match, compile(regexp))
}

//                                                               (success, newpos)
fun consume(string_to_match: string, pos: int, regexp: string) : (bool   ,    int)
{   
    consume(string_to_match, pos, compile(regexp))
}

//                                                                      (success, newpos, (sub_start, sub_end)[])
pure fun consume_n(string_to_match: string, pos: int, regexp: string) : (bool   ,    int, (int      , int    )[])
{
    consume_n(string_to_match, pos, compile(regexp))
}

//                                                                     (success, newpos, sub-matches [])
fun consume_n_str(string_to_match: string, pos: int, regexp: string) : (bool   ,    int,      string [])
{
    consume_n_str(string_to_match, pos, compile(regexp))
}

//                                                                        (success, newpos)
fun find_and_consume(string_to_match: string, pos: int, regexp: string) : (bool   ,    int)
{   
    find_and_consume(string_to_match, pos, compile(regexp))
}

//                                                                               (success, newpos, (sub_start, sub_end)[])
pure fun find_and_consume_n(string_to_match: string, pos: int, regexp: string) : (bool   ,    int, (int      , int    )[])
{
    find_and_consume_n(string_to_match, pos, compile(regexp))
}

//                                                                              (success, newpos, sub-matches [])
fun find_and_consume_n_str(string_to_match: string, pos: int, regexp: string) : (bool   ,    int,      string [])
{
    find_and_consume_n_str(string_to_match, pos, compile(regexp))
}

//                                                 (success, (sub_start, sub_end)[])
fun find(string_to_match: string, regexp: string): (bool   , (int      , int    )[])
{
    find(string_to_match, compile_for_find(regexp))
}

//                                                     (success, sub-matches[])
fun find_str(string_to_match: string, regexp: string): (bool   , string     [])
{
    find_str(string_to_match, compile_for_find(regexp))
}

//                                                    (success, (sub_start, sub_end)[,])
fun findall(string_to_match: string, regexp: string): (bool   , (int      , int    )[,])
{
    findall(string_to_match, compile_for_find(regexp))
}

//                                                        (success, sub-matches[,])
fun findall_str(string_to_match: string, regexp: string): (bool   , string     [,])
{
    findall_str(string_to_match, compile_for_find(regexp))
}

//                                                                    (success, result)
fun replace(str: string, regexp: string, rewrite: replace_pattern_t): (bool,    string)
{
    replace(str, compile_for_find(regexp), rewrite)
}

//                                                               (success, result)
fun replace(str: string, regexp: find_regex_t, rewrite: string): (bool,    string)
{
    replace(str, regexp, compile_replace_pattern(rewrite))
}

//                                                         (success, result)
fun replace(str: string, regexp: string, rewrite: string): (bool,    string)
{
    replace(str, compile_for_find(regexp), rewrite)
}

//                                                                    (success, result)
fun global_replace(str: string, regexp: string, rewrite: replace_pattern_t): (bool,    string)
{
    global_replace(str, compile_for_find(regexp), rewrite)
}

//                                                               (success, result)
fun global_replace(str: string, regexp: find_regex_t, rewrite: string): (bool,    string)
{
    global_replace(str, regexp, compile_replace_pattern(rewrite))
}

//                                                         (success, result)
fun global_replace(str: string, regexp: string, rewrite: string): (bool,    string)
{
    global_replace(str, compile_for_find(regexp), rewrite)
}
