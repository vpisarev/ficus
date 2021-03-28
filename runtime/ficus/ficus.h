/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_H__
#define __FICUS_H__

#include <assert.h>
#include <math.h>
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

/////////////////// Various Basic Definitions ////////////////

#define FX_INLINE static __inline
#ifdef __cplusplus
#define FX_EXTERN_C extern "C"
#define FX_EXTERN_C_VAL(decl) extern "C" {extern decl;}
#else
#define FX_EXTERN_C
#define FX_EXTERN_C_VAL(decl) extern decl;
#endif

typedef intptr_t int_; // int size in ficus is equal to the pointer size
#ifdef __cplusplus
typedef char32_t char_;
#else
#ifdef __APPLE__
typedef uint32_t char_;
#else
#include <uchar.h>
typedef char32_t char_;
#endif
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
    #elif defined __cplusplus
        #define FX_XADD(addr, delta) __atomic_fetch_add((intptr_t volatile *)(addr), delta, 4)
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

//////////////////////// Error Codes //////////////////////
extern int FX_EXN_ASCIIError;
extern int FX_EXN_AssertError;
extern int FX_EXN_BadArgError;
extern int FX_EXN_Break;
extern int FX_EXN_DimError;
extern int FX_EXN_DivByZeroError;
extern int FX_EXN_FileOpenError;
extern int FX_EXN_IOError;
extern int FX_EXN_NotFoundError;
extern int FX_EXN_NoMatchError;
extern int FX_EXN_NullFileError;
extern int FX_EXN_NullListError;
extern int FX_EXN_NullPtrError;
extern int FX_EXN_OptionError;
extern int FX_EXN_OutOfMemError;
extern int FX_EXN_OutOfRangeError;
extern int FX_EXN_OverflowError;
extern int FX_EXN_ParallelForError;
extern int FX_EXN_SizeError;
extern int FX_EXN_SizeMismatchError;
extern int FX_EXN_StackOverflowError;
extern int FX_EXN_SysBreak;
extern int FX_EXN_SysContinue;
extern int FX_EXN_RangeError;
extern int FX_EXN_TypeMismatchError;
extern int FX_EXN_UnknownExnError;
extern int FX_EXN_ZeroStepError;

#define FX_ZEROBUF_MAX_SIZE 256
extern const char fx_zerobuf[];

enum {
    FX_OK = 0,
    FX_EXN_StdMin = -48,
    FX_EXN_User = -1024
};

struct fx_str_t;

typedef void (*fx_free_t)(void*);
typedef void (*fx_copy_t)(const void*, void*);

int fx_init(int argc, char** argv);
int fx_init_thread(int t_idx);
int fx_deinit(int status);
int fx_cc_version(struct fx_str_t* ver);

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
#define FX_BREAK(label) ({ fx_status = FX_EXN_SysBreak; goto label; })
#define FX_CONTINUE(label) ({ fx_status = FX_EXN_SysContinue; goto label; })
#define FX_CHECK_CONTINUE() \
    if (fx_status != FX_EXN_SysContinue) ; else fx_status = FX_OK
#define FX_CHECK_BREAK() \
    if(fx_status != FX_EXN_SysBreak) ; else { \
        fx_status = FX_OK; \
        break; \
    }
#define FX_CHECK_BREAK_ND(br_label) \
    if(fx_status != FX_EXN_SysBreak) ; else { \
        fx_status = FX_OK; \
        goto br_label; \
    }
#define FX_CHECK_EXN(label) \
    if(fx_status >= 0) \
        ; \
    else goto label

#define FX_COPY_PTR(src, dst) if(src) { FX_INCREF((src)->rc); *(dst) = (src); } else *(dst) = 0
#define FX_COPY_SIMPLE(src, dst) *(dst) = (src)
#define FX_COPY_SIMPLE_BY_PTR(src, dst) *(dst) = *(src)
#define FX_NOP(ptr)
#define FX_CHECK_ZERO_STEP(delta, label) \
    if(delta == 0) ; else FX_FAST_THROW(FX_EXN_ZeroStepError, label)
#define FX_LOOP_COUNT(a, b, delta) \
    ((delta) > 0 ? ((b) - (a) + (delta) - 1)/(delta) : ((a) - (b) - (delta) - 1)/-(delta))
#define FX_CHECK_EQ_SIZE(check, label) if(check) ; else FX_FAST_THROW(FX_EXN_SizeMismatchError, label)
#define FX_CHECK_DIV_BY_ZERO(denom, label) if((denom) != 0) ; else FX_FAST_THROW(FX_EXN_DivByZeroError, label)

void fx_copy_ptr(const void* src, void* pdst);

///////////////////////////// Numbers ////////////////////////////

typedef union fx_bits64_t {double f; int64_t i; uint64_t u;} fx_bits64_t;
typedef union fx_bits32_t {float f; int i; unsigned u;} fx_bits32_t;

#if FX_ARCH_64

// use well-known trick that effectively rounds x by adding it to
// the magic constant with almost all zero mantissa bits and big-enough exponent.
// it looks like compilers are smart enough to avoid store/load operations;
// instead, they do "reinterpret_cast<int64_t>(x+magic_number)" right in the registers.
// in the end we propagate the sign bit of the 51-bit result.
FX_INLINE int_ fx_roundf2I(float x) {
    fx_bits64_t u;
    u.f = x + 6755399441055744.0;
    return (u.i << 13) >> 13;
}

FX_INLINE int_ fx_round2I(double x) {
    fx_bits64_t u;
    u.f = x + 6755399441055744.0;
    return (u.i << 13) >> 13;
}
#else
// on 32-bit machines we need just lower 32 bits of the result.
FX_INLINE int_ fx_roundf2I(float x) {
    fx_round64_t u;
    u.f = x + 6755399441055744.0;
    return (int_)(int)u.i;
}

FX_INLINE int_ fx_round2I(double x) {
    fx_bits64_t u;
    u.f = x + 6755399441055744.0;
    return (int_)(int)u.i;
}
#endif

FX_INLINE int fx_roundf2i(float x) {
    // special 32-bit version that can be converted to very
    // fast vectorized version inside vectorized loops.
    // the result is limited to [-2**22,2**22) value range,
    // it's still useful for float -> [u]int16 or [u]int8 conversion
    fx_bits32_t u;
    u.f = x + 12582912.0f;
    return (u.i << 10) >> 10;
}

FX_INLINE int fx_round2i(double x) {
    fx_bits64_t u;
    u.f = x + 6755399441055744.0;
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

int_ fx_strlen(const char_* rawstr);
bool fx_streq(const fx_str_t* a, const fx_str_t* b);
void fx_free_str(fx_str_t* str);
void fx_free_cstr(fx_cstr_t* cstr);
void fx_copy_str(const fx_str_t* src, fx_str_t* dst);
int fx_make_str(const char_* strdata, int_ length, fx_str_t* str);
int fx_make_cstr(const char* strdata, int_ length, fx_cstr_t* str);

#define FX_COPY_STR(src, dst) if((src)->rc) { FX_INCREF(*(src)->rc); *(dst) = *(src); } else *(dst) = *(src);
#define FX_FREE_STR(str) if(!(str)->rc) ; else fx_free_str(str)
#define FX_FREE_CSTR(cstr) if(!(cstr)->rc) ; else fx_free_cstr(cstr)
#define FX_MAKE_STR(strlit) { 0, (char_*)U##strlit, (int_)(sizeof(U##strlit)/sizeof(char_)-1) }
#define FX_MAKE_STR1(clit) { 0, (char_*)U##clit, 1 }
#define FX_MAKE_VAR_STR1(c) { 0, &c, 1 }
#define FX_CHAR(c) U##c

#define FX_STR_LENGTH(str) (str).length
#define FX_STR_CHKIDX(str, idx, catch_label) \
    if((size_t)(idx) < (size_t)(str).length) ; else FX_FAST_THROW(FX_EXN_OutOfRangeError, catch_label)
#define FX_STR_ELEM(str, idx) (str).data[idx]
#define FX_STR_ELEM_CLIP(str, idx) \
    ({ \
        fx_str_t* __str__ = &(str); \
        int_ __idx__ = (idx), __len__ = __str__->length; \
        (size_t)__idx__ < (size_t)__len__ ? __str__->data[__idx__] : \
        __len__ == 0 ? (char_)'\0' : __str__->data[__idx__ < 0 ? 0 : __len__-1]; \
    })
#define FX_STR_ELEM_ZERO(str, idx) \
    ({ \
        fx_str_t* __str__ = &(str); \
        int_ __idx__ = (idx); \
        ((size_t)__idx__ < (size_t)(__str__)->length) ? __str__->data[__idx__] : (char_)'\0'; \
    })

int fx_str2cstr(const fx_str_t* str, fx_cstr_t* cstr, char* buf, size_t bufsz);
size_t fx_str2cstr_slice(const fx_str_t* str, int_ start, int_ maxcount, char* buf);

int fx_ascii2str(const char* cstr, int_ length, fx_str_t* str);
int fx_cstr2str(const char* cstr, int_ length, fx_str_t* str);
int fx_substr(const fx_str_t* str, int_ start, int_ end, int_ delta, int mask, fx_str_t* substr);
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
bool fx_atoi(const fx_str_t* str, int_* result, int base);
bool fx_atof(const fx_str_t* str, double* result);
int fx_itoa(int64_t n, bool nosign, fx_str_t* str);

////////////////////////// Exceptions //////////////////////

typedef int (*fx_to_string_t)(void*, fx_str_t*, void*);
typedef int (*fx_print_t)(void*, void*);

struct fx_exn_data_t;
struct fx_exn_t;

typedef struct fx_exn_info_t
{
    fx_str_t name;
    fx_free_t free_f;
    fx_to_string_t to_string_f;
    fx_print_t print_repr_f;
} fx_exn_info_t;

#define FX_MAKE_EXN(exn, free_f_, to_string_, print_expr_) \
    { FX_MAKE_STR(#exn), free_f_, to_string_, print_repr_ };

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

#define FX_EXN_DATA(typ, data_) (((typ*)(data_))->data)

#define FX_SET_EXN_FAST(exn) \
    fx_exn_set_fast(exn, __func__, __FILE__, __LINE__)

#define FX_FAST_THROW(exn, catch_label) \
    { fx_status = FX_SET_EXN_FAST(exn); goto catch_label; }

#define FX_FAST_THROW_RET(exn) \
    return FX_SET_EXN_FAST(exn);

#define FX_SET_EXN(exn, move_exn) \
    fx_set_exn(exn, move_exn, __func__, __FILE__, __LINE__)

#define FX_THROW(exn, move_exn, catch_label) \
    { fx_status = FX_SET_EXN(exn, move_exn); goto catch_label; }
#define FX_RETHROW(exn, catch_label) \
    { fx_status = fx_rethrow_exn(exn); goto catch_label; }

#define FX_UPDATE_BT() fx_update_bt(__func__, __FILE__, __LINE__)

#define FX_MAKE_EXN_IMPL_START(exn_tag, exn_data_t, exn_info) \
    FX_DECL_AND_MALLOC(exn_data_t*, exn_data); \
    fx_result->tag = exn_tag; \
    fx_result->info = &exn_info; \
    fx_result->data = (fx_exn_data_t*)exn_data; \
    exn_data->rc = 1

int fx_exn_set_fast(int code, const char* funcname, const char* filename, int lineno);
int fx_set_exn(fx_exn_t* exn, bool move, const char* funcname, const char* filename, int lineno);
int fx_rethrow_exn(fx_exn_t* exn);
void fx_exn_get_and_reset(fx_exn_t* exn);
int fx_exn_check_parallel(int status, int* glob_status);
int fx_check_stack(void);

#define FX_CHECK_EXN_PARALLEL(status, par_status) \
    if((status) >= 0) ; else (status) = fx_exn_check_parallel((status), &(par_status))

const fx_exn_info_t* fx_exn_info(const fx_exn_t* exn);
int fx_exn_name(const fx_exn_t* exn, fx_str_t* exn_name);
int fx_exn_to_string(const fx_exn_t* exn, fx_str_t* str);
int fx_print_repr_exn(const fx_exn_t* exn, bool quiet);

void fx_update_bt(const char* funcname, const char* filename, int lineno);
void fx_print_bt(void);

void fx_free_exn(fx_exn_t* exn);
void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst);

#define FX_REG_SIMPLE_STD_EXN(tag, exn) \
    fx_register_simple_std_exn(tag, &exn)
#define FX_REG_SIMPLE_EXN(name, tag, info, exn) \
    fx_register_simple_exn(U##name, &tag, &info, &exn)
#define FX_REG_EXN(name, tag, info, free_f) \
    fx_register_exn(U##name, &tag, &info, (fx_free_t)free_f, 0, 0)

void fx_register_simple_exn(const char_* name, int* tag, fx_exn_info_t* info, fx_exn_t* exn);
void fx_register_simple_std_exn(int tag, fx_exn_t* exn);
void fx_register_exn(const char_* name, int* tag, fx_exn_info_t* info, fx_free_t free_f,
                    fx_to_string_t to_string_f, fx_print_t print_repr_f);

#define FX_FREE_EXN(exn) if(!(exn)->data) ; else fx_free_exn(exn)
#define FX_COPY_EXN(src, dst) if(!(src)->data) *(dst)=*(src) else fx_copy_exn((src), (dst))

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

int_ fx_list_length(const void* pl);
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
    bool reverse;
} fx_arriter_t;

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it, bool reverse);
void fx_arr_nextiter(fx_arriter_t* it);

#define FX_ARR_SIZE(arr, i) ((arr).dim[i].size)
#define FX_CHKIDX1(arr, i, idx) \
    ((size_t)(idx) < (size_t)(arr).dim[i].size)
#define FX_CHKIDX(ir_check, catch_label) \
    if(ir_check) ; else FX_FAST_THROW(FX_EXN_OutOfRangeError, catch_label)
#define FX_CHKIDX_SCALAR(arrsz, idx, catch_label) \
    FX_CHKIDX((uintptr_t)idx < (uintptr_t)arrsz, catch_label)
FX_INLINE int fx_check_idx_range(int_ arrsz, int_ a, int_ b, int_ delta, int_ scale, int_ shift)
{
    if (delta == 0) FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
    int n = FX_LOOP_COUNT(a, b, delta);
    if (n > 0) {
        int_ b_ = a + (n - 1)*delta;
        if((uintptr_t)(a*scale + shift) >= (uintptr_t)arrsz ||
           (uintptr_t)(b_*scale + shift) >= (uintptr_t)arrsz)
           FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    }
    return FX_OK;
}
#define FX_CHKIDX_RANGE(arrsz, a, b, delta, scale, shift, catch_label) \
    if(fx_check_idx_range(arrsz, a, b, delta, scale, shift) >= 0) ; \
    else goto catch_label

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

#define FX_CLIP_IDX(idx, sz) ((size_t)idx < (size_t)sz ? idx : idx < 0 ? 0 : sz-1)
#define FX_PTR_1D_CLIP(typ, arr, idx) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx__ = (idx), __sz0__ = __arr__->dim[0].size; \
        ((size_t)__idx__ < (size_t)__sz0__) ? FX_PTR_1D(typ, *__arr__, __idx0__) : \
        __sz0__ == 0 ? (typ*)fx_zerobuf : FX_PTR_1D(typ, *__arr__, (__idx__ < 0 ? 0 : __sz0__ - 1)); \
    ))
#define FX_PTR_2D_CLIP(typ, arr, idx0, idx1) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__) : \
        ((__sz0__ == 0) | (__sz1__ == 0)) ? (typ*)fx_zerobuf : \
        FX_PTR_2D(typ, *__arr__, FX_CLIP_IDX(__idx0__, __sz0__), \
            FX_CLIP_IDX(__idx1__, __sz1__)); \
    })
#define FX_PTR_3D_CLIP(typ, arr, idx0, idx1, idx2) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__) : \
        ((__sz0__ == 0) | (__sz1__ == 0) | (__sz2__ == 0)) ? (typ*)fx_zerobuf : \
        FX_PTR_2D(typ, *__arr__, FX_CLIP_IDX(__idx0__, __sz0__), \
            FX_CLIP_IDX(__idx1__, __sz1__), FX_CLIP_IDX(__idx2__, __sz2__)); \
    })
#define FX_PTR_4D_CLIP(typ, arr, idx0, idx1, idx2, idx3) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2), __idx3__ = (idx3); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size, __sz3__ = __arr__->dim[3].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__) & \
        ((size_t)__idx3__ < (size_t)__sz3__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__, __idx3__) : \
        ((__sz0__ == 0) | (__sz1__ == 0) | (__sz2__ == 0) | (__sz3__ == 0)) ? (typ*)fx_zerobuf : \
        FX_PTR_2D(typ, *__arr__, \
            FX_CLIP_IDX(__idx0__, __sz0__), \
            FX_CLIP_IDX(__idx1__, __sz1__), \
            FX_CLIP_IDX(__idx2__, __sz2__)  \
            FX_CLIP_IDX(__idx3__, __sz3__)); \
    })
#define FX_PTR_5D_CLIP(typ, arr, idx0, idx1, idx2, idx3, idx4) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2), \
            __idx3__ = (idx3), __idx4__ = (idx4); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size, __sz3__ = __arr__->dim[3].size; \
        int __sz4__ = __arr__->dim[4].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__) & \
        ((size_t)__idx3__ < (size_t)__sz3__) & \
        ((size_t)__idx4__ < (size_t)__sz4__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__, __idx3__, __idx4__) : \
        ((__sz0__ == 0) | (__sz1__ == 0) | (__sz2__ == 0) | (__sz3__ == 0) | (__sz4__ == 0)) ? \
            (typ*)fx_zerobuf : \
        FX_PTR_2D(typ, *__arr__, \
            FX_CLIP_IDX(__idx0__, __sz0__), \
            FX_CLIP_IDX(__idx1__, __sz1__), \
            FX_CLIP_IDX(__idx2__, __sz2__)  \
            FX_CLIP_IDX(__idx3__, __sz3__), \
            FX_CLIP_IDX(__idx4__, __sz4__)); \
    })

#define FX_PTR_1D_ZERO(typ, arr, idx) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx__ = (idx), __sz0__ = __arr__->dim[0].size; \
        (size_t)__idx__ < (size_t)__sz0__ ? ((typ*)__arr__->data + __idx__) : (typ*)fx_zerobuf; \
    ))
#define FX_PTR_2D_ZERO(typ, arr, idx0, idx1) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__) : (typ*)fx_zerobuf; \
    })
#define FX_PTR_3D_ZERO(typ, arr, idx0, idx1, idx2) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__) : (typ*)fx_zerobuf; \
    })
#define FX_PTR_4D_ZERO(typ, arr, idx0, idx1, idx2, idx3) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2), __idx3__ = (idx3); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size, __sz3__ = __arr__->dim[3].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__) & \
        ((size_t)__idx3__ < (size_t)__sz3__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__, __idx3__) : (typ*)fx_zerobuf; \
    })
#define FX_PTR_5D_ZERO(typ, arr, idx0, idx1, idx2, idx3, idx4) \
    ({ \
        fx_arr_t* __arr__ = &(arr); \
        int __idx0__ = (idx0), __idx1__ = (idx1), __idx2__ = (idx2), \
            __idx3__ = (idx3), __idx4__ = (idx4); \
        int __sz0__ = __arr__->dim[0].size, __sz1__ = __arr__->dim[1].size; \
        int __sz2__ = __arr__->dim[2].size, __sz3__ = __arr__->dim[3].size; \
        int __sz4__ = __arr__->dim[4].size; \
        (((size_t)__idx0__ < (size_t)__sz0__) & \
        ((size_t)__idx1__ < (size_t)__sz1__) & \
        ((size_t)__idx2__ < (size_t)__sz2__) & \
        ((size_t)__idx3__ < (size_t)__sz3__) & \
        ((size_t)__idx4__ < (size_t)__sz4__)) ? \
        FX_PTR_2D(typ, *__arr__, __idx0__, __idx1__, __idx2__, __idx3__, __idx4__) : (typ*)fx_zerobuf; \
    })

void fx_free_arr(fx_arr_t* arr);
#define FX_FREE_ARR(arr) if(!(arr)->rc) ; else fx_free_arr(arr)
#define FX_MOVE_ARR(src, dst) \
    { (dst) = (src); (src).rc = 0; (src).data = 0; }

void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst);
int fx_copy_arr_data(const fx_arr_t* src, fx_arr_t* dst, bool free_dst);
int fx_make_arr( int ndims, const int_* size, size_t elemsize,
                 fx_free_t free_elem, fx_copy_t copy_elem, const void* elems, fx_arr_t* arr );
int fx_compose_arr( int dims, size_t elemsize, fx_free_t free_elem, fx_copy_t copy_elem,
                    const int8_t* tags, const void** data, fx_arr_t* arr );
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

#define FX_REC_VARIANT_TAG(v) ((v) ? (v)->tag : 0)

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
#define FX_COPY_FP(src, dst) \
    if((src)->fcv && (src)->fcv->free_f) { \
        FX_INCREF((src)->fcv->rc); \
        *(dst) = *(src); \
    } else *(dst) = *(src)

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
void fx_pipe_destructor(void* ptr);

fx_cptr_t fx_get_stdstream(int);

////////////////////// Various useful system API ///////////////////

int64_t fx_tick_count(void);
double fx_tick_frequency(void);

////////////////////////// Regular expressions /////////////////////

typedef fx_cptr_t fx_regex_t;
int fx_re_compile(const fx_str_t* str, fx_regex_t* fx_result);
int fx_re_match(const fx_regex_t fx_regexp, const fx_str_t* str, bool* fx_result/*, fx_arr_t* fx_result_subs*/);
//int fx_re_find(const fx_regex_t fx_regexp, const fx_str_t* str, fx_arr_t* fx_result_subs);

#ifdef __cplusplus
}
#endif

#endif
