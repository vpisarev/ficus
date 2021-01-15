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

import List, String

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
}

type regex_t = { handle: cptr }

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

fun full_match(string_to_match : string, regexp : regex_t) : bool = ccode
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

//                                                                  (success, (sub_start, sub_end)[])
pure fun full_match_n(string_to_match : string, regexp : regex_t) : (bool   , (int      , int    )[]) = ccode
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

        const int_ arrdims[1] = {sub_amount};
        fx_result->t0 = RE2::FullMatchN(cstring_to_match.data, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!
        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            std::vector<int_> starts_and_ends(sub_amount * 2);
            for(int piece_num = 0; piece_num < sub_amount; piece_num++)
            {
                starts_and_ends[2 * piece_num    ] = sub_string_pieces[piece_num].begin() - cstring_to_match.data;
                starts_and_ends[2 * piece_num + 1] = sub_string_pieces[piece_num].end()   - cstring_to_match.data;
            }
            fx_status = fx_make_arr( 1, arrdims, sizeof(int_)+sizeof(int_), 0, 0, &(*(starts_and_ends.begin())), &(fx_result->t1));
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

pure fun full_match_n_str(string_to_match : string, regexp : regex_t) : (bool, string [])
{   
    val (success, starts_ends) = full_match_n(string_to_match, regexp)
    if(!success)
    {
        (false, array(0, ""))
    }
    else
    {
        (true, [for i <- 0: size(starts_ends)
                {
                    val (start, end) = starts_ends[i]
                    String.substr(string_to_match,start, end - start)
                }])
    }
}

pure fun partial_match(string_to_match : string, regexp : regex_t) : bool = ccode
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

//                                                                     (success, (sub_start, sub_end)[])
pure fun partial_match_n(string_to_match : string, regexp : regex_t) : (bool   , (int      , int    )[]) = ccode
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

        const int_ arrdims[1] = {sub_amount};
        fx_result->t0 = RE2::PartialMatchN(cstring_to_match.data, *re_to_apply, &(*sub_args.begin()), sub_amount); //TODO: try - catch!
        free_sub_args(sub_args);
        if(fx_result->t0)
        {
            std::vector<int_> starts_and_ends(sub_amount * 2);
            for(int piece_num = 0; piece_num < sub_amount; piece_num++)
            {
                starts_and_ends[2 * piece_num    ] = sub_string_pieces[piece_num].begin() - cstring_to_match.data;
                starts_and_ends[2 * piece_num + 1] = sub_string_pieces[piece_num].end()   - cstring_to_match.data;
            }
            fx_status = fx_make_arr( 1, arrdims, sizeof(int_)+sizeof(int_), 0, 0, &(*(starts_and_ends.begin())), &(fx_result->t1));
        }
        fx_free_cstr(&cstring_to_match);
    }
    return fx_status;
}

fun partial_match_n_str(string_to_match : string, regexp : regex_t) : (bool, string [])
{   
    val (success, starts_ends) = partial_match_n(string_to_match, regexp)
    if(!success)
    {
        (false, array(0, ""))
    }
    else
    {
        (true, [for i <- 0: size(starts_ends)
                {
                    val (start, end) = starts_ends[i]
                    String.substr(string_to_match,start, end - start)
                }])
    }
}


fun full_match(string_to_match : string, regexp : string) : bool
{   
    val reg = compile(regexp)
    full_match(string_to_match, reg) 
}

//                                                            (success, (sub_start, sub_end)[])
fun full_match_n(string_to_match : string, regexp : string) : (bool   , (int      , int    )[])
{
    val reg = compile(regexp)
    full_match_n(string_to_match, reg) 
}

fun full_match_n_str(string_to_match : string, regexp : string) : (bool, string [])
{
    val reg = compile(regexp)
    full_match_n_str(string_to_match, reg)
}

fun partial_match(string_to_match : string, regexp : string) : bool
{
    val reg = compile(regexp)
    partial_match(string_to_match, reg)
}

//                                                               (success, (sub_start, sub_end)[])
fun partial_match_n(string_to_match : string, regexp : string) : (bool   , (int      , int    )[])
{   
    val reg = compile(regexp)
    partial_match_n(string_to_match, reg)
}

fun partial_match_n_str(string_to_match : string, regexp : string) : (bool, string [])
{   
    val reg = compile(regexp)
    partial_match_n_str(string_to_match, reg)
}