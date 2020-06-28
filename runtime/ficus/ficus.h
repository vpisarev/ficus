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

#ifdef __cplusplus
extern "C" {
#endif

#if !defined FX_ARCH_32 && !defined FX_ARCH_64

#if (defined __SIZEOF_POINTER__ && __SIZEOF_POINTER__ == 8) || defined _M_ARM64 || defined _M_X64
#define FX_ARCH_64 1
#else
#define FX_ARCH_32 1
#endif

#endif

//////////////////////// Error Codes //////////////////////
enum
{
    FX_OK = 0,
    FX_EXN_Failure = -1,
    FX_EXN_AssertError = -2,
    FX_EXN_NotFoundError = -3,
    FX_EXN_OutOfMemError = -4,
    FX_EXN_OutOfRangeError = -5,
    FX_EXN_DivByZeroError = -6,
    FX_EXN_SizeMismatchError = -7,
    FX_EXN_TypeMismatchError = -8,
    FX_EXN_DimError = -9,
    FX_EXN_SizeError = -10,
    FX_EXN_FileOpenError = -11,
    FX_EXN_NullFileError = -12,
    FX_EXN_IOError = -13,
    FX_EXN_NoMatchError = -14,
    FX_EXN_Break = -15,
    FX_EXN_Continue = -16,
    FX_EXN_NullPtrError = -17,
    FX_EXN_ZeroStepError = -18,
    FX_EXN_ASCIIError = -19,
    FX_EXN_NullListError = -20,
    FX_EXN_OptionError = -21,
    FX_EXN_UnknownExnError = -22,

    FX_EXN_StdMax = -48,

    FX_EXN_User = -1024,
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
#if 0
    #define FX_XADD(addr, delta) ({ int_ prev = *(addr); *(addr) += (delta); prev; })
#else
    #ifdef _MSC_VER
        #include <intrin.h>
        #if defined _M_X64 || defined _M_ARM64
        #define FX_XADD(addr, delta) (int_)_InterlockedExchangeAdd64((__int64 volatile*)addr, delta)
        #else
        #define FX_XADD(addr, delta) (int)_InterlockedExchangeAdd((long volatile*)addr, delta)
        #endif
    #elif defined __clang__ && defined __ATOMIC_ACQ_REL
        #define FX_XADD(addr, delta) __c11_atomic_fetch_add((_Atomic(intptr_t)*)(addr), delta, __ATOMIC_ACQ_REL)
    #else
        #define FX_XADD(addr, delta) __atomic_fetch_add((_Atomic(intptr_t)*)(addr), delta, 4)
    #endif
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

int fx_init(int argc, char** argv);
int fx_init_thread(int t_idx);
int fx_finit(int status);

int_ fx_argc(void);
char* fx_argv(int_ idx);

void* fx_malloc(size_t sz);
void* fx_realloc(void* ptr, size_t sz);
void fx_free(void* ptr);

#define FX_DECL_AND_MALLOC(ptrtyp, ptr) \
    ptrtyp ptr = (ptrtyp)fx_malloc(sizeof(*ptr)); \
    if(!ptr) FX_FAST_THROW_RET(FX_EXN_OutOfMemError)
#define FX_RESULT_MALLOC(ptrtyp, ptr) \
    if (((ptr)=(ptrtyp)fx_malloc(sizeof(*(ptr)))) != 0) ; else FX_FAST_THROW_RET(FX_EXN_OutOfMemError)

#define FX_CALL(f, label) if((fx_status=(f)) < 0) { FX_UPDATE_BT(); goto label; } else fx_status=fx_status
// break/continue are execution flow control operators, not real exceptions,
// and they are guaranteed to be "caught" (one cannot place them outside of loops),
// so we don't use FX_SET_EXN_EXN_FAST() etc.
#define FX_BREAK(label) { fx_status = FX_EXN_Break; goto label; }
#define FX_CONTINUE(label) { fx_status = FX_EXN_Continue; goto label; }
#define FX_CHECK_CONTINUE() \
    if (fx_status != FX_EXN_Continue) ; else fx_status = FX_OK
#define FX_CHECK_BREAK() \
    if(fx_status != FX_EXN_Break) ; else { \
        fx_status = FX_OK; \
        break; \
    }
#define FX_CHECK_BREAK_ND(br_label) \
    if(fx_status != FX_EXN_Break) ; else { \
        fx_status = FX_OK; \
        goto br_label; \
    }
#define FX_CHECK_EXN(label) \
    if(fx_status >= 0) \
        ; \
    else goto label

#define FX_COPY_PTR(src, dst) { if(src) FX_INCREF((src)->rc); *(dst) = (src); }
#define FX_COPY_SIMPLE(src, dst) *(dst) = (src)
#define FX_COPY_SIMPLE_BY_PTR(src, dst) *(dst) = *(src)
#define FX_NOP(ptr)
#define FX_CHECK_ZERO_STEP(delta, label) \
    if(delta == 0) ; else FX_FAST_THROW(FX_EXN_ZeroStepError, label}
#define FX_LOOP_COUNT(a, b, delta) \
    ((delta) > 0 ? ((b) - (a) + (delta) - 1)/(delta) : ((a) - (b) - (delta) - 1)/-(delta))
#define FX_CHECK_EQ_SIZE(check, label) if(check) ; else { fx_status=FX_EXN_SizeMismatchError; goto label; }

void fx_copy_ptr(const void* src, void* pdst);

///////////////////////////// Numbers ////////////////////////////

typedef union fx_round64_t {double d; int64_t i;} fx_round64_t;
typedef union fx_round32_t {float f; int i;} fx_round32_t;

#if FX_ARCH_64

// use well-known trick that effectively rounds x by adding it to
// the magic constant with almost all zero mantissa bits and big-enough exponent.
// it looks like compilers are smart enough to avoid store/load operations;
// instead, they do "reinterpret_cast<int64_t>(x+magic_number)" right in the registers.
// in the end we propagate the sign bit of the 51-bit result.
FX_INLINE int_ fx_roundf2I(float x) {
    fx_round64_t u;
    u.d = x + 6755399441055744.0;
    return (u.i << 13) >> 13;
}

FX_INLINE int_ fx_round2I(double x) {
    fx_round64_t u;
    u.d = x + 6755399441055744.0;
    return (u.i << 13) >> 13;
}
#else
// on 32-bit machines we need just lower 32 bits of the result.
FX_INLINE int_ fx_roundf2I(float x) {
    fx_round64_t u;
    u.d = x + 6755399441055744.0;
    return (int_)(int)u.i;
}

FX_INLINE int_ fx_round2I(double x) {
    fx_round64_t u;
    u.d = x + 6755399441055744.0;
    return (int_)(int)u.i;
}
#endif

FX_INLINE int fx_roundf2i(float x) {
    // special 32-bit version that can be converted to very
    // fast vectorized version inside vectorized loops.
    // the result is limited to [-2**22,2**22) value range,
    // it's still useful for float -> [u]int16 or [u]int8 conversion
    fx_round32_t u;
    u.f = x + 12582912.0f;
    return (u.i << 10) >> 10;
}

FX_INLINE int fx_round2i(double x) {
    fx_round64_t u;
    u.d = x + 6755399441055744.0;
    return (int)u.i;
}

///////////////////////////// Strings ////////////////////////////

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
#define FX_CHAR(c) U##c

int fx_str2cstr(const fx_str_t* str, fx_cstr_t* cstr, char* buf, size_t bufsz);
size_t fx_str2cstr_slice(const fx_str_t* str, int_ start, int_ maxcount, char* buf);

int fx_ascii2str(const char* cstr, int_ length, fx_str_t* str);
int fx_cstr2str(const char* cstr, int_ length, fx_str_t* str);
int fx_substr(const fx_str_t* str, int_ start, int_ end, fx_str_t* substr);
int fx_strjoin(const fx_str_t* begin, const fx_str_t* end, const fx_str_t* sep,
                const fx_str_t* strs, int_ count, fx_str_t* result);

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
int fx_atoi(const fx_str_t* str, int_* result, bool* ok, int base);
int fx_itoa(int_ n, fx_str_t* str);

////////////////////////// Exceptions //////////////////////

struct fx_exn_data_t;
struct fx_exn_t;

typedef struct fx_exn_info_t
{
    int tag;
    fx_str_t name;
    fx_free_t free_f;
    int (*to_string)(const struct fx_exn_t* exn, fx_str_t* str);
    void (*print_repr)(const struct fx_exn_t* exn);
} fx_exn_info_t;

#define FX_DECL_EXN(tab, base, exn, free_f_, to_string_, print_repr_) \
{ \
    fx_exn_info_t temp = { exn, FX_MAKE_STR(#exn), free_f_, to_string_, print_repr_ }; \
    tab[base-exn] = temp; \
}

typedef struct fx_exn_data_t
{
    int_ rc;
} fx_exn_data_t;

typedef struct fx_exn_t
{
    int tag;
    struct fx_exn_info_t* info;
    struct fx_exn_data_t* data;
} fx_exn_t;

#define FX_SET_EXN_FAST(exn) \
    fx_exn_set_fast(exn, __func__, __FILE__, __LINE__)

#define FX_FAST_THROW(exn, catch_label) \
    { fx_status = FX_SET_EXN_FAST(exn); goto catch_label; }

#define FX_FAST_THROW_RET(exn) \
    return FX_SET_EXN_FAST(exn);

#define FX_SET_EXN(exn) \
    fx_set_exn(exn, true, __func__, __FILE__, __LINE__)

#define FX_UPDATE_BT() fx_update_bt(__func__, __FILE__, __LINE__)

int fx_set_exn_fast(int code, const char* funcname, const char* filename, int lineno);
int fx_set_exn(fx_exn_t* exn, bool move, const char* funcname, const char* filename, int lineno);
int fx_rethrow_exn(fx_exn_t* exn);
int fx_exn_get_and_reset(fx_exn_t* exn);

const fx_exn_info_t* fx_exn_info(const fx_exn_t* exn);
int fx_exn_name(const fx_exn_t* exn, fx_str_t* exn_name);
int fx_exn_to_string(const fx_exn_t* exn, fx_str_t* str);
int fx_print_repr_exn(const fx_exn_t* exn, bool quiet);

void fx_update_bt(const char* funcname, const char* filename, int lineno);
void fx_print_bt(void);

void fx_free_exn(fx_exn_t* exn);
void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst);

#define FX_FREE_EXN(exn) if(!(exn)->data) ; else fx_free_exn(exn)
#define FX_COPY_EXN(src, dst) if(!(src)->data) *(dst)=*(src) else fx_copy_exn((src), (dst))

#define FX_MAKE_EXN_IMPL(exn_tag, exn_data_typ, exndata_free, arg_copy_f) \
    FX_DECL_AND_MALLOC(exn_data_typ*, data); \
        \
    data->base.rc = 1; \
    data->base.free_f = exndata_free; \
    arg_copy_f(arg, &data->arg); \
        \
    fx_result->tag = exn_tag; \
    fx_result->data = (fx_exn_data_t*)data; \
    return FX_OK

//////////////////////////// Lists /////////////////////////

#define FX_FREE_LIST_IMPL(typ, hd_free_f) \
    typ l = *dst; \
    while(l) { \
        if(FX_DECREF(l->rc) > 1) \
            break; \
        typ tl = (typ)l->tl; \
        hd_free_f(&l->hd); \
        fx_free(l); \
        l = tl; \
    } \
    *dst = 0

#define FX_MAKE_LIST_IMPL(typ, hd_copy_f) \
    FX_DECL_AND_MALLOC(typ, l); \
    l->rc = 1; \
    l->tl = tl; \
    if(addref_tl && tl) FX_INCREF(tl->rc); \
    hd_copy_f(hd, &l->hd); \
    *fx_result = l; \
    return FX_OK

int_ fx_list_length(void* pl);
void fx_free_list_simple(void* pl);
void fx_link_lists(void* l1, void* l2, void* result);
#define FX_FREE_LIST_SIMPLE(pl) if(!*(pl)) ; else fx_free_list_simple(pl)

#define FX_LIST_APPEND(l_first, l_last, x) \
    if(l_last) l_last = l_last->tl = (x); else l_first = l_last = (x)

#define FX_MOVE_LIST(src, dst) \
    { (dst) = (src); (src) = 0; }

//////////////////////////// Arrays /////////////////////////

#define FX_MAX_DIMS 5
#define FX_ARR_CONTINUOUS 1
#define FX_IS_ARR_CONTINUOUS(flags) ((flags) & FX_ARR_CONTINUOUS)

typedef struct fx_arrdim_t
{
    int_ size;
    size_t step;
} fx_arrdim_t;

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

#define FX_ARR_SIZE(arr, i) ((arr).dim[i].size)
#define FX_CHKIDX1(arr, i, idx) \
    ((size_t)(idx) < (size_t)(arr).dim[i].size)
#define FX_CHKIDX(ir_check, catch_label) \
    if(ir_check) ; else FX_FAST_THROW(FX_EXN_OutOfRangeError, catch_label)

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
#define FX_MOVE_ARR(src, dst) \
    { (dst) = (src); (src).rc = 0; (src).data = 0; }

void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst);
int fx_copy_arr_data(const fx_arr_t* src, fx_arr_t* dst);
int fx_make_arr( int ndims, const int_* size, size_t elemsize,
                 fx_free_t free_elem, fx_copy_t copy_elem, const void* elems, fx_arr_t* arr );
int fx_subarr(const fx_arr_t* arr, const int_* ranges, fx_arr_t* result);
int fx_flatten_arr(const fx_arr_t* arr, fx_arr_t* farr);

////////////////////////// References //////////////////////////

#define FX_FREE_REF_IMPL(typ, arg_free_f) \
    typ r = *dst; \
    if(r && FX_DECREF(r->rc) == 1) \
    { \
        arg_free_f(&r->data); \
        fx_free(r); \
    } \
    *dst = 0

#define FX_MAKE_REF_IMPL(typ, arg_copy_f) \
    FX_DECL_AND_MALLOC(typ, r); \
    r->rc = 1; \
    arg_copy_f(arg, &r->data); \
    *fx_result = r; \
    return FX_OK

void fx_free_ref_simple(void* pr);
#define FX_FREE_REF_SIMPLE(pr) if(!*(pr)) ; else fx_free_ref_simple(pr)

/////////////////////////// Variants /////////////////////////

#define FX_MAKE_RECURSIVE_VARIANT_IMPL_START(variant_ptr_t) \
    FX_DECL_AND_MALLOC(variant_ptr_t, v); \
    *fx_result = v; \
    v->rc = 1

//////////////////////// Function Pointers/Closures /////////////////////////

typedef struct fx_fcv_t
{
    int_ rc;
    fx_free_t free_f;
} fx_fcv_t;

#define FX_FREE_FP(f) \
    if((f)->fcv) { \
        if((f)->fcv->free_f && FX_DECREF((f)->fcv->rc) == 1) \
            (f)->fcv->free_f((f)->fcv); \
        (f)->fcv=0; \
    }
#define FX_COPY_FP(src, dst) { \
    if((src)->fcv && (src)->fcv->free_f) \
        FX_INCREF((src)->fcv->rc); \
    *(dst) = *(src); \
}

#define FX_MAKE_FP_IMPL_START(fcv_t, free_f_, fname) \
    FX_DECL_AND_MALLOC(fcv_t*, fcv); \
    fx_result->fp = fname; \
    fx_result->fcv = (fx_fcv_t*)fcv; \
    fcv->rc = 1; \
    fcv->free_f = (fx_free_t)free_f_

void fx_free_fp(void* fp);
void fx_copy_fp(const void* src, void* pdst);

///////////////////////////// C pointers ///////////////////////////

typedef struct fx_cptr_data_t
{
    int_ rc;
    fx_free_t free_f;
    void* ptr;
} fx_cptr_data_t, *fx_cptr_t;

void fx_cptr_no_free(void* ptr);
void fx_free_cptr(fx_cptr_t* cptr);
void fx_copy_cptr(const fx_cptr_t src, fx_cptr_t* pdst);
int fx_make_cptr(void* ptr, fx_free_t free_f, fx_cptr_t* fx_result);

//////////////////////////// File I/O //////////////////////////////

int fx_fputs(FILE* f, const fx_str_t* str);
int fx_fgets(FILE* f, fx_str_t* str);
void fx_file_destructor(void* ptr);

fx_cptr_t fx_get_stdstream(int);

#ifdef __cplusplus
}
#endif

#endif
