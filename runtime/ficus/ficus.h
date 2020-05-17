/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_H__
#define __FICUS_H__

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ficus/version.h"

//////////////////////// Error Codes //////////////////////
enum
{
    FX_OK = 0,
    FX_FAILURE = -1,
    FX_OUT_OF_MEM_ERR = -2,
    FX_INDEX_ERR = -3,
    FX_DIV_BY_ZERO_ERR = -4,
    FX_SIZE_MISMATCH_ERR = -5,
    FX_DIM_ERR = -6,
    FX_SIZE_ERR = -7,
    FX_FILE_OPEN_ERR = -8,
    FX_NULL_FILE_ERR = -9,
    FX_IO_ERR = -10,
    FX_BREAK_ERR = -11,
    FX_CONTINUE_ERR = -12,
    FX_NULLPTR_ERR = -13,
};

/////////////////// Various Basic Definitions ////////////////

#define FX_INLINE __inline

typedef intptr_t int_; // int size in ficus is equal to the pointer size
#ifdef __APPLE__
typedef uint32_t char_;
#else
#include <uchar.h>
typedef char32_t char_;
#endif

#ifndef FX_XADD
#ifdef _MSC_VER
#include <intrin.h>
#if defined _M_X64 || defined _M_ARM64
#define FX_XADD(addr, delta) (int_)_InterlockedExchangeAdd64((__int64 volatile*)addr, delta)
#else
#define FX_XADD(addr, delta) (int)_InterlockedExchangeAdd((long volatile*)addr, delta)
#endif
#elif defined __ATOMIC_ACQ_REL
#define FX_XADD(addr, delta) __c11_atomic_fetch_add((_Atomic(intptr_t)*)(addr), delta, __ATOMIC_ACQ_REL)
#else
#define FX_XADD(addr, delta) __atomic_fetch_add((_Atomic(intptr_t)*)(addr), delta, 4)
#endif
#endif

#define FX_INCREF(rc) FX_XADD(&(rc), 1)
#define FX_DECREF(rc) FX_XADD(&(rc), -1)

#ifdef _MSC_VER
#define FX_THREAD_LOCAL __declspec(thread)
#else
#define FX_THREAD_LOCAL __thread
#endif

typedef void (*fx_free_t)(void*);
typedef void (*fx_copy_t)(const void*, void*);

typedef struct fx_rng_t
{
    uint64_t state;
} fx_rng_t;

struct fx_exndata_t;

typedef struct fx_exndata_t
{
    int_ rc;
    fx_free_t free_f;
} fx_exndata_t;

typedef struct fx_exn_t
{
    int tag;
    struct fx_exndata_t* data;
} fx_exn_t;

extern FX_THREAD_LOCAL fx_exn_t fx_exn;
extern FX_THREAD_LOCAL fx_rng_t fx_rng;

void fx_init(int argc, char** argv);
void fx_init_thread(int t_idx);
int_ fx_argc(void);
char* fx_argv(int_ idx);

void* fx_malloc(size_t sz);
void* fx_realloc(void* ptr, size_t sz);
void fx_free(void* ptr);
#define FX_DECL_AND_MALLOC(typ, ptr, sz) \
    typ* ptr = (typ*)fx_malloc(sz); \
    if(!ptr) return FX_OUT_OF_MEM_ERR
#define FX_CALL(f, label) fx_status = f; if(fx_status < 0) goto label
#define FX_BREAK(label) fx_status = FX_BREAK_ERR; goto label
#define FX_CONTINUE(label) fx_status = FX_CONTINUE_ERR; goto label
#define FX_LOOP_CATCH_BREAK_CONTINUE(label) \
    if(fx_status >= 0) \
        ; \
    else if(fx_status == FX_BREAK_ERR) { \
        fx_status = FX_OK; \
        break; \
    } \
    else if(fx_status == FX_CONTINUE_ERR) { \
        fx_status = FX_OK; \
        continue; \
    } \
    else goto label

#define FX_LOOP_CATCH(label) \
    if(fx_status >= 0) \
        ; \
    else goto label

#define FX_COPY_PTR(src, dst) FX_INCREF((src)->rc); *(dst) = (src)
#define FX_COPY_SIMPLE(src, dst) *(dst) = (src)
#define FX_COPY_SIMPLE_BY_PTR(src, dst) *(dst) = *(src)
#define FX_NOP(ptr)

void fx_copy_ptr(const void* src, void* pdst);

////////////////////////// Strings //////////////////////

// Unicode character category
enum
{
    FX_UNICODE_CAT_Lu = 0,
    FX_UNICODE_CAT_Ll = 1,
    FX_UNICODE_CAT_Lt = 2,
    FX_UNICODE_CAT_Lm = 3,
    FX_UNICODE_CAT_Lo = 4,

    FX_UNICODE_CAT_Mn = 5,
    FX_UNICODE_CAT_Me = 6,
    FX_UNICODE_CAT_Mc = 7,

    FX_UNICODE_CAT_Nd = 8,
    FX_UNICODE_CAT_Nl = 9,
    FX_UNICODE_CAT_No = 10,

    FX_UNICODE_CAT_Zs = 11,
    FX_UNICODE_CAT_Zl = 12,
    FX_UNICODE_CAT_Zp = 13,

    FX_UNICODE_CAT_Cc = 14,
    FX_UNICODE_CAT_Cf = 15,
    FX_UNICODE_CAT_Co = 16,
    FX_UNICODE_CAT_Cs = 17,
    FX_UNICODE_CAT_Cn = 18,

    FX_UNICODE_CAT_Pd = 19,
    FX_UNICODE_CAT_Ps = 20,
    FX_UNICODE_CAT_Pe = 21,
    FX_UNICODE_CAT_Pc = 22,
    FX_UNICODE_CAT_Po = 23,
    FX_UNICODE_CAT_Pi = 24,
    FX_UNICODE_CAT_Pf = 25,

    FX_UNICODE_CAT_Sm = 26,
    FX_UNICODE_CAT_Sc = 27,
    FX_UNICODE_CAT_Sk = 28,
    FX_UNICODE_CAT_So = 29,
    FX_UNICODE_CAT_Zextra = 30, // extra space (TAB, CR, LF etc.)
    FX_UNICODE_CAT_Unknown = FX_UNICODE_CAT_Cn,
    FX_UNICODE_CAT_Mask = 31,
    FX_UNICODE_CAT_Shift = 5
};

// Unicode character bidirectional category
enum
{
    FX_UNICODE_BIDIR_AL = 0,
    FX_UNICODE_BIDIR_AN = 1,
    FX_UNICODE_BIDIR_B = 2,
    FX_UNICODE_BIDIR_BN = 3,
    FX_UNICODE_BIDIR_CS = 4,
    FX_UNICODE_BIDIR_EN = 5,
    FX_UNICODE_BIDIR_ES = 6,
    FX_UNICODE_BIDIR_ET = 7,
    FX_UNICODE_BIDIR_FSI = 8,
    FX_UNICODE_BIDIR_L = 9,
    FX_UNICODE_BIDIR_LRE = 10,
    FX_UNICODE_BIDIR_LRI = 11,
    FX_UNICODE_BIDIR_LRO = 12,
    FX_UNICODE_BIDIR_NSM = 13,
    FX_UNICODE_BIDIR_ON = 14,
    FX_UNICODE_BIDIR_PDF = 15,
    FX_UNICODE_BIDIR_PDI = 16,
    FX_UNICODE_BIDIR_R = 17,
    FX_UNICODE_BIDIR_RLE = 18,
    FX_UNICODE_BIDIR_RLI = 19,
    FX_UNICODE_BIDIR_RLO = 20,
    FX_UNICODE_BIDIR_S = 21,
    FX_UNICODE_BIDIR_WS = 22,
    FX_UNICODE_BIDIR_Mask = 31,
    FX_UNICODE_BIDIR_Shift = 5
};

typedef struct fx_str_t
{
    int_* rc;
    char_* data;
    int_ length;
} fx_str_t;

// this type is not exposed in Ficus language,
// but used by the runtime and the standard library
typedef struct fx_cstr_t
{
    int_* rc;
    char* data;
    int_ length;
} fx_cstr_t;

void fx_free_str(fx_str_t* str);
void fx_free_cstr(fx_cstr_t* cstr);
void fx_copy_str(const fx_str_t* src, fx_str_t* dst);
int fx_make_str(const char_* strdata, int_ length, fx_str_t* str);
#define FX_FREE_STR(str) if(!(str)->rc) ; else fx_free_str(str)
#define FX_FREE_CSTR(cstr) if(!(cstr)->rc) ; else fx_free_cstr(cstr)
#define FX_MAKE_STR(strlit) { 0, U##strlit, (int_)(sizeof(U##strlit)/sizeof(char_)-1) }

int fx_str2cstr(const fx_str_t* str, fx_cstr_t* cstr, char* buf, size_t bufsz);
size_t _fx_str2cstr_slice(const fx_str_t* str, int_ start, int_ maxcount, char* buf);

int fx_cstr2str(const char* cstr, int_ length, fx_str_t* str);
int fx_substr(const fx_str_t* str, int_ start, int_ end, fx_str_t* substr);
int fx_strjoin(const fx_str_t* sep, fx_str_t* strs, int_ count, fx_str_t* result);

bool fx_isalpha(char_ ch);
bool fx_isdigit(char_ ch);
bool fx_isalnum(char_ ch);
bool fx_ispunct(char_ ch);
bool fx_isdecimal(char_ ch);
bool fx_isspace(char_ ch);
char_ fx_tolower(char_ ch);
char_ fx_toupper(char_ ch);
int fx_todigit(char_ ch);
int fx_bidirectional(char_ ch);
int fx_atoi(const fx_str_t* str, int* result, bool* ok, int base);

////////////////////////// Exceptions //////////////////////

#define FX_THROW_LIGHT(exn_name, catch_label) \
    fx_status = exn_name; goto catch_label

void fx_free_exn(fx_exn_t* exn);
void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst);

#define FX_FREE_EXN(exn) if(!(exn)->data) ; else fx_free_exn(exn)
#define FX_COPY_EXN(src, dst) if(!(src)->data) *(dst)=*(src) else fx_copy_exn((src), (dst))

#define FX_MAKE_EXN_IMPL(exn_tag, exndata_typ, exndata_free, arg_copy_f) \
    FX_DECL_AND_MALLOC(exndata_typ, data, sizeof(exndata_typ)); \
        \
    data->base.rc = 1; \
    data->base.free_f = exndata_free; \
    arg_copy_f(arg, &data->arg); \
        \
    fx_result->tag = exn_tag; \
    fx_result->data = (fx_exndata_t*)data; \
    return FX_OK

//////////////////////////// Lists /////////////////////////

#define FX_FREE_LIST_IMPL(typ, hd_free_f) \
    typ l = *pl; \
    while(l) { \
        if(FX_DECREF(l->rc) > 1) \
            break; \
        typ tl = (typ)l->tl; \
        hd_free_f(&l->hd); \
        fx_free(l); \
        l = tl; \
    } \
    *pl = 0

#define FX_MAKE_LIST_IMPL(typ, hd_copy_f) \
    FX_DECL_AND_MALLOC(typ, l, sizeof(typ)); \
    l->rc = 1; \
    l->tl = tl; \
    if(addref_tl && tl) FX_INCREF(tl->rc); \
    hd_copy_f(hd, &l->hd); \
    *fx_result = l; \
    return FX_OK

void fx_free_list_simple(void* pl);
#define FX_FREE_LIST_SIMPLE(pl) if(!*(pl)) ; else fx_free_list_simple(pl)

//////////////////////////// Arrays /////////////////////////

#define FX_MAX_DIMS 5
#define FX_ARR_CONTINUOUS 1
#define FX_IS_ARR_CONTINUOUS(flags) ((flags) & FX_ARR_CONTINUOUS)

typedef struct fx_arrdim_t
{
    int_ size;
    size_t step;
} fx_arrdim_t;

typedef void (*fx_free_t)(void* elem);
typedef void (*fx_copy_t)(const void* src, void* dst);

typedef struct fx_arr_t
{
    int_* rc;
    fx_free_t free_elem;
    fx_copy_t copy_elem;
    int flags;
    int ndims;
    // put 'data' together with the interleaved '(size, step)' pairs
    // in order to improve the cache locality, e.g. in the case of
    // 2D array element access we just need to read 4
    // sequentially stored elements:
    // data, dim[0].size, dim[0].step, dim[1].size
    char*  data;
    fx_arrdim_t dim[FX_MAX_DIMS];
}
fx_arr_t;

typedef struct fx_arriter_t
{
    int ndims;
    int narrays;
    int_ nblocks;
    int_ blocksize;
    fx_arr_t** arrs;
    char** ptrs;
    int iterdepth;
    int_ idx;
} fx_arriter_t;

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it);
void fx_arr_nextiter(fx_arriter_t* it);

#define FX_CHKIDX1(arr, i, idx) \
    ((size_t)(idx) >= (size_t)(arr).dim[i].size)
#define FX_CHKIDX(oor_check, catch_label) \
    if(oor_check) { fx_status = FX_INDEX_ERR; goto catch_label; }

#define FX_PTR_1D(typ, arr, idx) \
    ((typ*)(arr).data + (idx))
#define FX_PTR_2D(typ, arr, idx0, idx1) \
    ((typ*)((arr).data + (arr).dim[0].step*(idx0)) + (idx1))
#define FX_PTR_3D(typ, arr, idx0, idx1, idx2) \
    ((typ*)((arr).data + (arr).dim[0].step*(idx0) + \
    (arr).dim[1].step*(idx1)) + (idx2))
#define FX_PTR_4D(typ, arr, idx0, idx1, idx2, idx3) \
    ((typ*)((arr).data + (arr).dim[0].step*(idx0) + \
    (arr).dim[1].step*(idx1) + (arr).dim[2].step*(idx2)) + (idx3))
#define FX_PTR_5D(typ, arr, idx0, idx1, idx2, idx3) \
    ((typ*)((arr).data + (arr).dim[0].step*(idx0) + \
    (arr).dim[1].step*(idx1) + (arr).dim[2].step*(idx2) + \
    (arr).dim[3].step*(idx3)) + (idx4))

void fx_free_arr(fx_arr_t* arr);
#define FX_FREE_ARR(arr) if(!(arr)->rc) ; else fx_free_arr(arr)

void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst);
int fx_make_arr( int ndims, const int_* size, size_t elemsize,
                 fx_free_t free_elem, fx_copy_t copy_elem, const void* elems, fx_arr_t* arr );
int fx_subarr(const fx_arr_t* arr, const int_* ranges, fx_arr_t* subarr);

////////////////////////// References //////////////////////////

#define FX_FREE_REF_IMPL(typ, arg_free_f) \
    typ r = *pr; \
    if(r && FX_DECREF(r->rc) == 1) \
    { \
        arg_free_f(&r->data); \
        fx_free(r); \
    } \
    *pr = 0

#define FX_MAKE_REF_IMPL(typ, arg_copy_f) \
    FX_DECL_AND_MALLOC(typ, r, sizeof(typ)); \
    r->rc = 1; \
    arg_copy_f(arg, &r->data); \
    *fx_result = r; \
    return FX_OK

void fx_free_ref_simple(void* pr);
#define FX_FREE_REF_SIMPLE(pr) if(!*(pr)) ; else fx_free_list_simple(pr)

//////////////////////// Function pointers /////////////////////////

typedef struct fx_base_fv_t
{
    int_ rc;
    fx_free_t free_f;
} fx_base_fv_t;

typedef struct fx_fv_t
{
    fx_base_fv_t base;
} fx_fv_t;

typedef struct fx_fp_t
{
    void (*fp)(void);
    fx_fv_t* fv;
} fx_fp_t;

#define FX_FREE_FP(f) \
    if((f)->fv) { (f)->fv->base.free_f((f)->fv); (f)->fv=0; }
#define FX_COPY_FP(src, dst) \
    if((src)->fv) FX_INCREF((src)->fv->base.rc); *(dst) = *(src)

void fx_free_fp(void* fp);
void fx_copy_fp(const void* src, void* pdst);

///////////////////////////// C pointers ///////////////////////////

typedef struct fx_cptr_cell_t
{
    int_ rc;
    fx_free_t free_f;
    void* ptr;
} fx_cptr_cell_t, *fx_cptr_t;

void fx_cptr_no_free(void* ptr);
void fx_free_cptr(fx_cptr_t* cptr);
void fx_copy_cptr(const fx_cptr_t src, fx_cptr_t* pdst);
int fx_make_cptr(void* ptr, fx_free_t free_f, fx_cptr_t* fx_result);

//////////////////////////// File I/O //////////////////////////////

int fx_fputs(FILE* f, const fx_str_t* str);
int fx_fgets(FILE* f, fx_str_t* str);

fx_cptr_t fx_get_stdin(void);
fx_cptr_t fx_get_stdout(void);
fx_cptr_t fx_get_stderr(void);

#endif
