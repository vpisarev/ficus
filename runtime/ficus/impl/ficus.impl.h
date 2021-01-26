/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_IMPL_H__
#define __FICUS_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

/////////////////////////// initialization /////////////////////

static int _fx_argc = 0;
static char** _fx_argv = 0;

int_ fx_argc(void) { return _fx_argc; }
char* fx_argv(int_ idx) { return _fx_argv[idx]; }

static fx_exn_info_t fx_std_exn_info[-FX_EXN_StdMax];

int FX_EXN_ASCIIError = -1;
int FX_EXN_AssertError = -2;
int FX_EXN_Break = -3;
int FX_EXN_DimError = -4;
int FX_EXN_DivByZeroError = -5;
int FX_EXN_FileOpenError = -6;
int FX_EXN_IOError = -7;
int FX_EXN_NotFoundError = -8;
int FX_EXN_NoMatchError = -9;
int FX_EXN_NullFileError = -10;
int FX_EXN_NullListError = -11;
int FX_EXN_NullPtrError = -12;
int FX_EXN_OptionError = -13;
int FX_EXN_OutOfMemError = -14;
int FX_EXN_OutOfRangeError = -15;
int FX_EXN_RangeError = -16;
int FX_EXN_SizeError = -17;
int FX_EXN_SizeMismatchError = -18;
int FX_EXN_SysBreak = -19;
int FX_EXN_SysContinue = -20;
int FX_EXN_TypeMismatchError = -21;
int FX_EXN_UnknownExnError = -22;
int FX_EXN_ZeroStepError = -23;
int FX_EXN_StackOverflowError = -24;
int FX_EXN_ParallelForError = -25;
int FX_EXN_BadArgError = -26;

int fx_init(int argc, char** argv)
{
    _fx_argc = argc;
    _fx_argv = argv;

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
    FX_DECL_STD_EXN(TypeMismatchError);
    FX_DECL_STD_EXN(UnknownExnError);
    FX_DECL_STD_EXN(ZeroStepError);
    FX_DECL_STD_EXN(StackOverflowError);
    FX_DECL_STD_EXN(ParallelForError);
    FX_DECL_STD_EXN(BadArgError);

    #undef FX_DECL_STD_EXN

    return fx_init_thread(0);
}

static FX_THREAD_LOCAL bool fx_is_main_thread = false;
static FX_THREAD_LOCAL char* fx_stack_top = 0;
static int_ FX_MAX_STACK_SIZE = 1000000;

int fx_init_thread(int t_idx)
{
    if(t_idx == 0)
        fx_is_main_thread = true;
    char local_val = 0;
    fx_stack_top = &local_val;
    return FX_OK;
}

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

int fx_deinit(int status)
{
    if(status < 0)
        fx_print_bt();
    return status;
}

////////////////////////// memory allocation ////////////////////

/* [TODO] replace it with something more efficient,
   e.g. mimalloc (https://github.com/microsoft/mimalloc) or
   jemalloc or tcmalloc */
void* fx_malloc(size_t sz)
{
    return malloc(sz);
}

void* fx_realloc(void* ptr, size_t sz)
{
    return realloc(ptr, sz);
}

void fx_free(void* ptr)
{
    free(ptr);
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
    FX_BT_HALF_SIZE = 16
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
    int istack_top;
    int ostack_top;
    int ostack_bottom;
    fx_bt_entry_t istack[FX_BT_HALF_SIZE];
    fx_bt_entry_t ostack[FX_BT_HALF_SIZE];
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
    fx_bt_entry_t* curr_loc = &curr_bt->istack[0];
    curr_loc->funcname = funcname;
    curr_loc->filename = filename;
    curr_loc->lineno = lineno;
    curr_bt->istack_top = 1;
    curr_bt->ostack_top = curr_bt->ostack_bottom = 0;
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
    fx_bt_entry_t* curr_loc = &curr_bt->istack[0];
    curr_loc->funcname = funcname;
    curr_loc->filename = filename;
    curr_loc->lineno = lineno;
    curr_bt->istack_top = 1;
    curr_bt->ostack_top = curr_bt->ostack_bottom = 0;
    return exn->tag;
}

// ligher version of above:
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
void fx_exn_get_and_reset(fx_exn_t* exn)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    *exn = *curr_exn;
    curr_exn->data = 0;
}

int fx_exn_check_parallel(int status, int* glob_status)
{
    // [TODO] try to return the original exception when it's possible
    if(status < 0)
    {
        fx_bt_t* curr_bt = &fx_bt;
        fx_free_exn(&curr_bt->curr_exn);
        *glob_status = FX_EXN_ParallelForError;
    }
    return 0;
}

const fx_exn_info_t* fx_exn_info(const fx_exn_t* exn)
{
    if(exn->info) return exn->info;
    int tag = exn->tag;
    if(tag > 0 || tag < FX_EXN_StdMax) return 0;
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

void fx_print_bt(void)
{
    fx_bt_t* curr_bt = &fx_bt;
    fx_exn_t* curr_exn = &curr_bt->curr_exn;
    if(curr_exn->tag == 0) return;
    printf("\33[31;1mException ");
    fx_print_repr_exn(curr_exn, true);
    printf("\33[0m occured in ");
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
    FX_DECL_AND_MALLOC(fx_cptr_t, p);
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

#endif
