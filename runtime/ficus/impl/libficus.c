/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
#include "ficus/ficus.h"

#if defined _WIN32 || defined WINCE
    #define FX_WINDOWS 1
    #ifndef _WIN32_WINNT           // This is needed for the declaration of TryEnterCriticalSection in winbase.h with Visual Studio 2005 (and older?)
    #define _WI N32_WINNT 0x0400  // http://msdn.microsoft.com/en-us/library/ms686857(VS.85).aspx
    #endif
    #include <windows.h>
#else
    #define FX_UNIX 1
    #include <unistd.h>
    #include <time.h>
    #include <execinfo.h>
    #if defined __MACH__ && defined __APPLE__
    #include <mach/mach.h>
    #include <mach/mach_time.h>
    #endif
#endif

#include "ficus/impl/rpmalloc.h"

#ifdef __ARM_NEON
#include <arm_neon.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/////////////////////////// initialization /////////////////////

static int _fx_argc = 0;
static char** _fx_argv = 0;

int_ fx_argc(void) { return _fx_argc; }
char* fx_argv(int_ idx) { return _fx_argv[idx]; }
int fx_get_max_threads(void) {
    int omp_get_max_threads();
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}

static fx_exn_info_t fx_std_exn_info[-FX_EXN_StdMin];

int FX_EXN_ASCIIError = -1;
int FX_EXN_AssertError = -2;
int FX_EXN_BadArgError = -3;
int FX_EXN_Break = -4;
int FX_EXN_DimError = -5;
int FX_EXN_DivByZeroError = -6;
int FX_EXN_FileOpenError = -7;
int FX_EXN_IOError = -8;
int FX_EXN_NotFoundError = -9;
int FX_EXN_NotImplementedError = -10;
int FX_EXN_NoMatchError = -11;
int FX_EXN_NullFileError = -12;
int FX_EXN_NullListError = -13;
int FX_EXN_NullPtrError = -14;
int FX_EXN_OptionError = -15;
int FX_EXN_OutOfMemError = -16;
int FX_EXN_OutOfRangeError = -17;
int FX_EXN_OverflowError = -18;
int FX_EXN_ParallelForError = -19;
int FX_EXN_RangeError = -20;
int FX_EXN_SizeError = -21;
int FX_EXN_SizeMismatchError = -22;
int FX_EXN_StackOverflowError = -23;
int FX_EXN_SysBreak = -24;
int FX_EXN_SysContinue = -25;
int FX_EXN_SysReturn = -26;
int FX_EXN_TypeMismatchError = -27;
int FX_EXN_UnknownExnError = -28;
int FX_EXN_ZeroStepError = -29;

int fx_init(int argc, char** argv)
{
    _fx_argc = argc;
    _fx_argv = argv;

    rpmalloc_initialize();

    #undef FX_DECL_STD_EXN
    #define FX_DECL_STD_EXN(exn) \
        { fx_exn_info_t tmp = {FX_MAKE_STR(#exn), 0, 0, 0}; fx_std_exn_info[-FX_EXN_##exn]=tmp; }

    fx_str_t okstr = FX_MAKE_STR("OK");
    fx_std_exn_info[0].name = okstr;

    FX_DECL_STD_EXN(ASCIIError);
    FX_DECL_STD_EXN(AssertError);
    FX_DECL_STD_EXN(Break);
    FX_DECL_STD_EXN(DimError);
    FX_DECL_STD_EXN(DivByZeroError);
    FX_DECL_STD_EXN(FileOpenError);
    FX_DECL_STD_EXN(IOError);
    FX_DECL_STD_EXN(NotFoundError);
    FX_DECL_STD_EXN(NotImplementedError);
    FX_DECL_STD_EXN(NoMatchError);
    FX_DECL_STD_EXN(NullFileError);
    FX_DECL_STD_EXN(NullListError);
    FX_DECL_STD_EXN(NullPtrError);
    FX_DECL_STD_EXN(OptionError);
    FX_DECL_STD_EXN(OutOfMemError);
    FX_DECL_STD_EXN(OutOfRangeError);
    FX_DECL_STD_EXN(RangeError);
    FX_DECL_STD_EXN(SizeError);
    FX_DECL_STD_EXN(SizeMismatchError);
    FX_DECL_STD_EXN(SysBreak);
    FX_DECL_STD_EXN(SysContinue);
    FX_DECL_STD_EXN(SysReturn);
    FX_DECL_STD_EXN(TypeMismatchError);
    FX_DECL_STD_EXN(UnknownExnError);
    FX_DECL_STD_EXN(ZeroStepError);
    FX_DECL_STD_EXN(StackOverflowError);
    FX_DECL_STD_EXN(ParallelForError);
    FX_DECL_STD_EXN(BadArgError);
    FX_DECL_STD_EXN(OverflowError);

    #undef FX_DECL_STD_EXN

    return fx_init_thread(0);
}

static FX_THREAD_LOCAL bool fx_is_main_thread = false;
static FX_THREAD_LOCAL char* fx_stack_top = 0;
static int_ FX_MAX_STACK_SIZE = 4 << 20;

int fx_init_thread(int t_idx)
{
    if(t_idx == 0)
        fx_is_main_thread = true;
    char local_val = 0;
    fx_stack_top = &local_val;
    return FX_OK;
}

#ifndef FX_NO_STACK_OVERFLOW_CHECK
int fx_check_stack(void)
{
    if (fx_is_main_thread) {
        char local_val = 0;
        char* stack_bottom = &local_val;
        char* stack_top = fx_stack_top;
        if (stack_bottom < stack_top && stack_top - stack_bottom > FX_MAX_STACK_SIZE)
            FX_FAST_THROW_RET(FX_EXN_StackOverflowError);
    }
    return FX_OK;
}
#endif

int fx_deinit(int status)
{
    rpmalloc_finalize();
    return status;
}

int fx_cc_version(struct fx_str_t* ver)
{
#ifdef __VERSION__
#if (defined __clang__) || !(defined __GNUC__)
    char cver[] = __VERSION__;
#else
    char cver[] = "GCC " __VERSION__;
#endif
#elif defined _MSC_VER
    char cver[128];
    int revision =
    #ifdef _MSC_BUILD
        _MSC_BUILD;
    #else
        0;
    #endif
    int fullver =
    #ifdef _MSC_FULL_VER
        _MSC_FULL_VER;
    #else
        _MSC_VER*10000;
    #endif
    if (fullver / 10000 == _MSC_VER) {
        fullver *= 100000;
    }
    int major = fullver / 10000000;
    int minor = (fullver % 10000000) / 100000;
    int build = fullver % 100000;
    sprintf(cver, "Microsoft MSVC %d.%02d.%05d.%d", major, minor, build, revision);
#elif defined __STDC_VERSION__
    char cver[128];
    int stdc_ver = (int)__STDC_VERSION__;
    sprintf(cver, "ISO %d.%d-complaint C compiler", (int)stdc_ver/100, (int)stdc_ver%100);
#else
    char cver[] = "Unknown C compiler"
#endif
    return fx_cstr2str(cver, -1, ver);
}

////////////////////////// memory allocation ////////////////////

static FX_THREAD_LOCAL volatile bool fx_rpmalloc_thread_initialized = false;

void* fx_malloc(size_t sz)
{
    if (!fx_rpmalloc_thread_initialized) {
        rpmalloc_thread_initialize();
        fx_rpmalloc_thread_initialized = true;
    }
    return rpmalloc(sz);
}

void* fx_realloc(void* ptr, size_t sz)
{
    if (!fx_rpmalloc_thread_initialized) {
        rpmalloc_thread_initialize();
        fx_rpmalloc_thread_initialized = true;
    }
    return rprealloc(ptr, sz);
}

void fx_free(void* ptr)
{
    if (!fx_rpmalloc_thread_initialized) {
        rpmalloc_thread_initialize();
        fx_rpmalloc_thread_initialized = true;
    }
    rpfree(ptr);
}

///////////////////////////// lists /////////////////////////////

typedef struct fx_list_simple_data_t
{
    int_ rc;
    struct fx_list_simple_data_t* tl;
    int_ hd;
}* fx_list_simple_t;

void fx_free_list_simple(void* dst_)
{
    fx_list_simple_t *dst = (fx_list_simple_t*)dst_;
    FX_FREE_LIST_IMPL(fx_list_simple_t, FX_NOP);
}

int_ fx_list_length(const void* pl_)
{
    fx_list_simple_t pl = (fx_list_simple_t)pl_;
    int_ len = 0;
    for(; pl != 0; pl = pl->tl)
        len++;
    return len;
}

void fx_link_lists(void* l1_, void* l2_, void* result_)
{
    fx_list_simple_t l1 = (fx_list_simple_t)l1_;
    fx_list_simple_t l2 = (fx_list_simple_t)l2_;
    fx_list_simple_t* result = (fx_list_simple_t*)result_;

    if(!l1)
        l1 = l2;
    else {
        fx_list_simple_t tmp = l1;
        while(tmp->tl) tmp=tmp->tl;
        FX_COPY_PTR(l2, &tmp->tl);
    }
    FX_COPY_PTR(l1, result);
}

///////////////////////// references /////////////////////////

typedef struct fx_ref_simple_data_t
{
    int_ rc;
}* fx_ref_simple_t;

void fx_free_ref_simple(void* dst_)
{
    fx_ref_simple_t *dst = (fx_ref_simple_t*)dst_;
    FX_FREE_REF_IMPL(fx_ref_simple_t, FX_NOP);
}

////////////////// reference-counted cells ////////////////////

void fx_copy_ptr(const void* src, void* dst)
{
    int_* src_ = (int_*)src;
    int_** dst_ = (int_**)dst;
    if(src_) FX_INCREF(*src_);
    *dst_ = src_;
}

///////////////////////// exceptions //////////////////////////

void fx_free_exn(fx_exn_t* exn)
{
    if(exn->data)
    {
        if(FX_DECREF(exn->data->rc) == 1)
            exn->info->free_f(exn->data);
        exn->data = 0;
    }
}

void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst)
{
    if(src->data) FX_INCREF(src->data->rc);
    *dst = *src;
}

enum
{
    FX_BT_HALF_SIZE = 16,
    FX_BT_SIZE = FX_BT_HALF_SIZE*2
};

typedef struct fx_bt_entry_t
{
    const char* funcname;
    const char* filename;
    int lineno;
} fx_bt_entry_t;

typedef struct fx_bt_t
{
    fx_exn_t curr_exn;

#if FX_USE_UPDATE_BT
    int istack_top;
    int ostack_top;
    int ostack_bottom;
    fx_bt_entry_t istack[FX_BT_HALF_SIZE];
    fx_bt_entry_t ostack[FX_BT_HALF_SIZE];
#else
    fx_bt_entry_t thrown_at;
    int bt_entries;
    void* bt_stack[FX_BT_SIZE];
#endif
} fx_bt_t;

static FX_THREAD_LOCAL fx_bt_t fx_bt;

int fx_exn_set_fast(int code, const char* funcname, const char* filename, int lineno)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    assert(curr_exn->data == 0);
    curr_exn->tag = code;
    curr_exn->info = 0;
    curr_exn->data = 0; // well, it must be NULL already
#if FX_USE_UPDATE_BT
    fx_bt_entry_t* curr_loc = &curr_bt->istack[0];
    curr_bt->istack_top = 1;
    curr_bt->ostack_top = curr_bt->ostack_bottom = 0;
#else
    fx_bt_entry_t* curr_loc = &curr_bt->thrown_at;
#ifdef FX_UNIX
    curr_bt->bt_entries = backtrace(curr_bt->bt_stack, FX_BT_SIZE);
#else
    //curr_bt->bt_entries = CaptureStackBackTrace(0, FX_BT_SIZE, curr_bt->bt_stack, 0);
#endif
#endif
    curr_loc->funcname = funcname;
    curr_loc->filename = filename;
    curr_loc->lineno = lineno;
    return code;
}

int fx_set_exn(fx_exn_t* exn, bool move, const char* funcname, const char* filename, int lineno)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    assert(curr_exn->data == 0);
    *curr_exn = *exn;
    if(move)
        exn->data = 0;
    else if(exn->data)
        FX_INCREF(exn->data->rc);
#if FX_USE_UPDATE_BT
    fx_bt_entry_t* curr_loc = &curr_bt->istack[0];
    curr_bt->istack_top = 1;
    curr_bt->ostack_top = curr_bt->ostack_bottom = 0;
#else
    fx_bt_entry_t* curr_loc = &curr_bt->thrown_at;
#ifdef FX_UNIX
    curr_bt->bt_entries = backtrace(curr_bt->bt_stack, FX_BT_SIZE);
#else
    curr_bt->bt_entries = CaptureStackBackTrace(0, FX_BT_SIZE, curr_bt->bt_stack, 0);
#endif
#endif
    curr_loc->funcname = funcname;
    curr_loc->filename = filename;
    curr_loc->lineno = lineno;
    return exn->tag;
}

// lighter version of above:
//   1. rethrowing exception always means moving it
//   2. do not reset backtrace stack
int fx_rethrow_exn(fx_exn_t* exn)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    assert(curr_exn->data == 0);
    *curr_exn = *exn;
    exn->data = 0;
    return curr_exn->tag;
}

// it's always used in the beginning of "catch"
void fx_exn_get_and_reset(int fx_status, fx_exn_t* exn)
{
    if (FX_EXN_SysReturn <= fx_status && fx_status <= FX_EXN_SysBreak)
    {
        exn->tag = fx_status;
        exn->info = &fx_std_exn_info[-fx_status];
        exn->data = 0;
    }
    else
    {
        fx_bt_t* curr_bt = &fx_bt;
        fx_exn_t* curr_exn = &curr_bt->curr_exn;
        *exn = *curr_exn;
        curr_exn->data = 0;
    }
}
/*void fx_exn_get_and_reset(fx_exn_t* exn)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    *exn = *curr_exn;
    curr_exn->data = 0;
}*/

int fx_exn_check_parallel(int status, int* glob_status)
{
    if(status < 0)
    {
        fx_bt_t* curr_bt = &fx_bt;
        fx_free_exn(&curr_bt->curr_exn);
        *glob_status = FX_EXN_ZeroStepError <= status && status <= FX_EXN_ASCIIError ?
                status : FX_EXN_ParallelForError;
    }
    return 0;
}

const fx_exn_info_t* fx_exn_info(const fx_exn_t* exn)
{
    if(exn->info) return exn->info;
    int tag = exn->tag;
    if(tag > 0 || tag <= FX_EXN_StdMin) return 0;
    const fx_exn_info_t* info = &fx_std_exn_info[-tag];
    return info;
}

int fx_exn_name(const fx_exn_t* exn, fx_str_t* exn_name)
{
    const fx_exn_info_t* info = fx_exn_info(exn);
    if(!info) FX_FAST_THROW_RET(FX_EXN_UnknownExnError);
    // exception name is a literal, defined with FX_MAKE_STR(),
    // so there is no need to "incref" it.
    *exn_name = info->name;
    return FX_OK;
}

int fx_exn_to_string(const fx_exn_t* exn, fx_str_t* str)
{
    const fx_exn_info_t* info = fx_exn_info(exn);
    if(!info) FX_FAST_THROW_RET(FX_EXN_UnknownExnError);
    if(info->to_string_f) return info->to_string_f((fx_exn_t*)exn, str, 0);
    // if there is no dedicated to_string_f function, just return the name
    *str = info->name;
    return FX_OK;
}

int fx_print_repr_exn(const fx_exn_t* exn, bool quiet)
{
    const fx_exn_info_t* info = fx_exn_info(exn);
    if(!info) {
        if(quiet) {
            printf("Unknown_exception(%d)", exn->tag);
            return FX_OK;
        }
        FX_FAST_THROW_RET(FX_EXN_UnknownExnError);
    }
    if(info->print_repr_f)
        info->print_repr_f((fx_exn_t*)exn, 0);
    else
        // if there is no dedicated to_string function, just print the name
        fx_fputs(stdout, &info->name);
    return FX_OK;
}

typedef struct fx_exn_exit_data_t
{
    int_ rc;
    int_ data;
} fx_exn_exit_data_t;

#if FX_USE_UPDATE_BT
void fx_print_bt_entry(const fx_bt_entry_t* entry, const char* prefix)
{
    const char* filename = entry->filename;
    const char* barename1 = strrchr(filename, '/');
    const char* barename2 = strrchr(filename, '\\');
    const char* barename = barename1 > barename2 ? barename1 : barename2;
    barename = barename ? barename + 1 : filename;
    printf("%s%s at %s:%d\n", prefix, entry->funcname, barename, entry->lineno);
}

void fx_update_bt(const char* funcname, const char* filename, int lineno)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_bt_entry_t* currloc;
    int itop = curr_bt->istack_top;
    if(itop < FX_BT_HALF_SIZE) {
        currloc = curr_bt->istack + itop;
        curr_bt->istack_top = itop+1;
    } else {
        int otop = curr_bt->ostack_top;
        currloc = curr_bt->ostack + otop;
        otop = (otop + 1) % FX_BT_HALF_SIZE;
        if(curr_bt->ostack_bottom == otop)
            curr_bt->ostack_bottom = (otop + 1) % FX_BT_HALF_SIZE;
        curr_bt->ostack_top = otop;
    }
    currloc->funcname = funcname;
    currloc->filename = filename;
    currloc->lineno = lineno;
}
#endif

int fx_print_bt(void)
{
    fx_str_t curr_exn_name;
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    if(curr_exn->tag == 0) return 0;
    fx_exn_name(curr_exn, &curr_exn_name);
    if (curr_exn_name.length == 4 &&
        memcmp(curr_exn_name.data, U"Exit", 4*sizeof(char_)) == 0) {
        int status = (int)((fx_exn_exit_data_t*)(curr_exn->data))->data;
        return status;
    }
    printf("\33[31;1mException ");
    fx_print_repr_exn(curr_exn, true);
    printf("\33[0m occured in ");

#if FX_USE_UPDATE_BT
    for (int i = 0; i < curr_bt->istack_top; i++) {
        fx_print_bt_entry(curr_bt->istack+i,
            i == 0 ? "" : "\tcalled from ");
    }

    if(curr_bt->ostack_top != curr_bt->ostack_bottom) {
        printf("\t...\n");
        for(int i = curr_bt->ostack_bottom; i != curr_bt->ostack_top; i = (i + 1) % FX_BT_HALF_SIZE) {
            fx_print_bt_entry(curr_bt->ostack+i, "\tcalled from");
        }
    }
#else
    const char* filename = curr_bt->thrown_at.filename;
    const char* barename1 = strrchr(filename, '/');
    const char* barename2 = strrchr(filename, '\\');
    const char* barename = barename1 > barename2 ? barename1 : barename2;
    barename = barename ? barename + 1 : filename;
    printf("%s at %s:%d\n", curr_bt->thrown_at.funcname, barename, curr_bt->thrown_at.lineno);

#ifdef FX_UNIX
    if (curr_bt->bt_entries > 2) {
        char** symbols = backtrace_symbols(curr_bt->bt_stack+2, curr_bt->bt_entries-2);
        printf("called from:\n");
        for (int i = 0; i < curr_bt->bt_entries-2; i++) {
            printf("\t%s\n", symbols[i]);
        }
    }
#endif
#endif
    return curr_exn->tag;
}

static int fx_global_exn_id = FX_EXN_User;

void fx_register_exn(const char_* name, int* tag, fx_exn_info_t* info, fx_free_t free_f,
                    fx_to_string_t to_string_f, fx_print_t print_repr_f)
{
    fx_str_t nstr = {0, (char_*)name, fx_strlen(name)};
    info->name = nstr;
    info->free_f = free_f;
    info->to_string_f = to_string_f;
    info->print_repr_f = print_repr_f;

    *tag = fx_global_exn_id--;
}

void fx_register_simple_exn(const char_* name, int* tag, fx_exn_info_t* info, fx_exn_t* exn)
{
    fx_register_exn(name, tag, info, 0, 0, 0);
    exn->tag = *tag;
    exn->info = info;
    exn->data = 0;
}

void fx_register_simple_std_exn(int tag, fx_exn_t* exn)
{
    exn->tag = tag;
    if(FX_EXN_StdMin < tag && tag <= 0) {
        exn->info = &fx_std_exn_info[-tag];
    } else {
        assert(0);
    }
}

//////////////// function pointers ////////////////

typedef struct fx_fp_t
{
    void (*fp)(void);
    fx_fcv_t* fcv;
} fx_fp_t;

void fx_free_fp(void* fp)
{
    fx_fp_t* fp_ = (fx_fp_t*)fp;
    FX_FREE_FP(fp_)
}

void fx_copy_fp(const void* src, void* pdst)
{
    fx_fp_t *src_ = (fx_fp_t*)src, **pdst_ = (fx_fp_t**)pdst;
    FX_COPY_FP(src_, *pdst_);
}

//////////////////// interfaces ///////////////////

typedef struct fx_iface_vtbl_t
{
    fx_free_t __free__;
} fx_iface_vtbl_t;

typedef struct fx_iface_t
{
    const fx_iface_vtbl_t* vtbl;
    fx_object_t* obj;
} fx_iface_t;

void fx_free_iface(void* iface)
{
    fx_iface_t* iface_ = (fx_iface_t*)iface;
    FX_FREE_IFACE(iface_);
}

void fx_copy_iface(const void* src, void* dst)
{
    const fx_iface_t *src_ = (const fx_iface_t*)src;
    fx_iface_t *dst_ = (fx_iface_t*)dst;
    FX_COPY_IFACE(src_, dst_);
}

int fx_make_iface(const void* obj, int idx, void* iface)
{
    fx_object_t* obj_ = (fx_object_t*)obj;
    fx_iface_t* iface_ = (fx_iface_t*)iface;
    if (!obj_ || !iface_)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    if (idx < 0 || idx >= obj_->ifaces->nifaces)
        return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
    iface_->vtbl = (const fx_iface_vtbl_t*)obj_->ifaces->ifaces[idx].vtbl;
    FX_INCREF(obj_->rc);
    iface_->obj = obj_;
    return FX_OK;
}

int fx_query_iface(const void* iface, int iface_id, void* another_iface)
{
    fx_iface_t* iface_ = (fx_iface_t*)iface;
    fx_object_t* iobj = iface_ ? iface_->obj : 0;
    fx_iface_t* another_iface_ = (fx_iface_t*)another_iface;
    if (!iface_ || !iobj)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    for(int i = 0; i < iobj->ifaces->nifaces; i++ ) {
        if (iobj->ifaces->ifaces[i].iface_id == iface_id) {
            another_iface_->vtbl = (const fx_iface_vtbl_t*)iobj->ifaces->ifaces[i].vtbl;
            another_iface_->obj = iobj;
            FX_INCREF(iobj->rc);
            return FX_OK;
        }
    }
    return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
}

int fx_get_object(const void* iface, int idx, void* obj)
{
    fx_iface_t* iface_ = (fx_iface_t*)iface;
    fx_object_t* iobj = iface_ ? iface_->obj : 0;
    if (!iface_ || !iobj)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    if (idx < 0 || idx >= iobj->ifaces->nifaces ||
        iobj->ifaces->ifaces[idx].vtbl != (void*)iface_->vtbl)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    FX_INCREF(iobj->rc);
    *(void**)obj = iobj;
    return FX_OK;
}

static int fx_global_iface_id = -1;
void fx_register_iface(int* iface_id)
{
    *iface_id = ++fx_global_iface_id;
}

void fx_init_ifaces(void* free_f, int nifaces, const int* iface_ids,
                    const fx_iface_entry_t* entries, fx_ifaces_t* ifaces)
{
    ifaces->free_f = (fx_free_t)free_f;
    ifaces->nifaces = nifaces;
    ifaces->ifaces = (fx_iface_entry_t*)entries;
    for(int i = 0; i < nifaces; i++) {
        ifaces->ifaces[i].iface_id = iface_ids[i];
    }
}

//////////////////// cpointers ////////////////////

void fx_cptr_no_free(void* ptr) {}

void fx_free_cptr(fx_cptr_t* pcptr)
{
    if(*pcptr)
    {
        fx_cptr_t cptr = *pcptr;
        if(cptr->free_f && FX_DECREF(cptr->rc) == 1) {
            cptr->free_f(cptr->ptr);
            fx_free(cptr);
        }
        *pcptr = 0;
    }
}

void fx_copy_cptr(const fx_cptr_t src, fx_cptr_t* dst)
{
    if(src && src->free_f) FX_INCREF(src->rc);
    *dst = src;
}

int fx_make_cptr(void* ptr, fx_free_t free_f, fx_cptr_t* fx_result)
{
    fx_cptr_t p = (fx_cptr_t)fx_malloc(sizeof(*p));
    if (!p) {
        if (free_f && ptr)
            free_f(ptr);
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    p->rc = 1;
    p->free_f = free_f;
    p->ptr = ptr;
    *fx_result = p;
    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#include "ficus/impl/array.impl.h"
#include "ficus/impl/file.impl.h"
#include "ficus/impl/string.impl.h"
#include "ficus/impl/regex.impl.h"
#include "ficus/impl/system.impl.h"
#include "ficus/impl/gemm.impl.h"
#include "ficus/impl/rrbvec.impl.h"
#include "ficus/impl/rpmalloc.impl.h"
