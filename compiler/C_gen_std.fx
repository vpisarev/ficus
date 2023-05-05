/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Defines some standard ficus runtime functions and macros
*/
from Ast import *
from C_form import *

fun gen_std_fun(cname: string, argtyps: ctyp_t list, rt: ctyp_t)
{
    val n = gen_idc(1, cname)
    val cf = ref (cdeffun_t {
        cf_name=n,
        cf_rt=rt,
        cf_args=[:: for t <- argtyps {(noid, t, [])}],
        cf_cname=cname,
        cf_body=[],
        cf_flags=default_fun_flags().{fun_flag_ccode=true, fun_flag_pure=0},
        cf_scope=[],
        cf_loc=noloc
        })
    set_idc_entry(n, CFun(cf))
    n
}

fun gen_std_macro(cname: string, nargs: int)
{
    val n = gen_idc(1, cname)
    val cm = ref (cdefmacro_t {cm_name=n, cm_cname=cname,
                cm_args=[:: for i <- 0:nargs {noid} ],
                cm_body=[], cm_scope=[], cm_loc=noloc})
    set_idc_entry(n, CMacro(cm))
    n
}

fun init_std_names(): void {
    std_sizeof = gen_std_fun("sizeof", [:: CTypAny], CTypSize_t)
    std_fx_malloc = gen_std_fun("fx_malloc", [:: CTypSize_t, std_CTypVoidPtr], CTypCInt)
    std_fx_free = gen_std_fun("fx_free", [:: std_CTypVoidPtr], CTypVoid)
    std_fx_free_t = CTypName(get_id("fx_free_t"))
    std_fx_copy_t = CTypName(get_id("fx_copy_t"))
    std_FX_INCREF = gen_std_macro("FX_INCREF", 1)
    std_FX_DECREF = gen_std_macro("FX_DECREF", 1)
    std_FX_REC_VARIANT_TAG = gen_std_macro("FX_REC_VARIANT_TAG", 1)
    std_FX_MAKE_RECURSIVE_VARIANT_IMPL_START = gen_std_macro("FX_MAKE_RECURSIVE_VARIANT_IMPL_START", 1)
    std_FX_MAKE_FP_IMPL_START = gen_std_macro("FX_MAKE_FP_IMPL_START", 3)
    std_FX_MAKE_FP_BY_FCV = gen_std_macro("FX_MAKE_FP_BY_FCV", 3)
    std_FX_CALL = gen_std_macro("FX_CALL", 2)
    std_FX_COPY_PTR = gen_std_macro("FX_COPY_PTR", 2)
    std_FX_COPY_SIMPLE = gen_std_macro("FX_COPY_SIMPLE", 2)
    std_FX_COPY_SIMPLE_BY_PTR = gen_std_macro("FX_COPY_SIMPLE_BY_PTR", 2)
    std_FX_NOP = gen_std_macro("FX_NOP", 1)
    std_FX_BREAK = gen_std_macro("FX_BREAK", 1)
    std_FX_CONTINUE = gen_std_macro("FX_CONTINUE", 1)
    std_FX_CHECK_BREAK = gen_std_macro("FX_CHECK_BREAK", 0)
    std_FX_CHECK_BREAK_ND = gen_std_macro("FX_CHECK_BREAK_ND", 1)
    std_FX_CHECK_CONTINUE = gen_std_macro("FX_CHECK_CONTINUE", 0)
    std_FX_CHECK_EXN = gen_std_macro("FX_CHECK_EXN", 1)
    std_FX_CHECK_ZERO_STEP = gen_std_macro("FX_CHECK_ZERO_STEP", 2)
    std_FX_LOOP_COUNT = gen_std_macro("FX_LOOP_COUNT", 3)
    std_FX_CHECK_EQ_SIZE = gen_std_macro("FX_CHECK_EQ_SIZE", 2)
    std_fx_copy_ptr = gen_std_fun("fx_copy_ptr", [:: std_CTypConstVoidPtr, std_CTypVoidPtr ], CTypVoid)
    std_FX_STR_LENGTH = gen_std_macro("FX_STR_LENGTH", 1)
    std_FX_STR_CHKIDX = gen_std_macro("FX_STR_CHKIDX", 3)
    std_FX_STR_ELEM = gen_std_macro("FX_STR_ELEM", 2)
    std_FX_STR_ELEM_CLIP = gen_std_macro("FX_STR_ELEM_CLIP", 2)
    std_FX_STR_ELEM_ZERO = gen_std_macro("FX_STR_ELEM_ZERO", 2)
    std_FX_MAKE_STR = gen_std_macro("FX_MAKE_STR", 1)
    std_FX_FREE_STR = gen_std_macro("FX_FREE_STR", 1)
    std_FX_COPY_STR = gen_std_macro("FX_COPY_STR", 2)
    std_fx_free_str = gen_std_fun("fx_free_str", [:: make_ptr(CTypString)], CTypVoid)
    std_fx_copy_str = gen_std_fun("fx_copy_str", [:: make_const_ptr(CTypString), make_ptr(CTypString) ], CTypVoid)
    std_fx_substr = gen_std_fun("fx_substr", [:: make_ptr(CTypString), CTypInt, CTypInt, CTypInt, CTypCInt, make_ptr(CTypString) ], CTypVoid)
    std_FX_FREE_LONG = gen_std_macro("FX_FREE_LONG", 1)
    std_FX_COPY_LONG = gen_std_macro("FX_COPY_LONG", 2)
    std_fx_free_long = gen_std_fun("fx_free_long", [:: make_ptr(CTypLong)], CTypVoid)
    std_fx_copy_long = gen_std_fun("fx_copy_long", [:: make_const_ptr(CTypLong), make_ptr(CTypLong) ], CTypVoid)
    std_fx_exn_info_t = CTypName(get_id("fx_exn_info_t"))
    std_FX_REG_SIMPLE_EXN = gen_std_macro("FX_REG_SIMPLE_EXN", 4)
    std_FX_REG_SIMPLE_STD_EXN = gen_std_macro("FX_REG_SIMPLE_STD_EXN", 2)
    std_FX_REG_EXN = gen_std_macro("FX_REG_EXN", 4)
    std_FX_MAKE_EXN_IMPL_START = gen_std_macro("FX_MAKE_EXN_IMPL_START", 3)
    std_FX_THROW = gen_std_macro("FX_THROW", 3)
    std_FX_RETHROW = gen_std_macro("FX_RETHROW", 2)
    std_FX_FAST_THROW = gen_std_macro("FX_FAST_THROW", 2)
    std_FX_FREE_EXN = gen_std_macro("FX_FREE_EXN", 1)
    std_FX_COPY_EXN = gen_std_macro("FX_COPY_EXN", 2)
    std_FX_MAKE_EXN_IMPL = gen_std_macro("FX_EXN_MAKE_IMPL", 4)
    std_fx_free_exn = gen_std_fun("fx_free_exn", [:: make_ptr(CTypExn)], CTypVoid)
    std_fx_copy_exn = gen_std_fun("fx_copy_exn", [:: make_const_ptr(CTypExn), make_ptr(CTypExn)], CTypVoid)
    std_FX_FREE_LIST_SIMPLE = gen_std_macro("FX_FREE_LIST_SIMPLE", 1)
    std_fx_free_list_simple = gen_std_fun("fx_free_list_simple", [:: std_CTypVoidPtr], CTypVoid)
    std_fx_list_length = gen_std_fun("fx_list_length", [:: std_CTypVoidPtr], CTypInt)
    std_FX_FREE_LIST_IMPL = gen_std_macro("FX_FREE_LIST_IMPL", 2)
    std_FX_MAKE_LIST_IMPL = gen_std_macro("FX_MAKE_LIST_IMPL", 2)
    std_FX_LIST_APPEND = gen_std_macro("FX_LIST_APPEND", 3)
    std_FX_MOVE_LIST = gen_std_macro("FX_MOVE_LIST", 2)
    std_FX_CHKIDX1 = gen_std_macro("FX_CHKIDX1", 3)
    std_FX_CHKIDX = gen_std_macro("FX_CHKIDX", 2)
    std_FX_PTR_xD = []
    std_FX_PTR_xD_CLIP = []
    std_FX_PTR_xD_ZERO = []
    for i <- std_FX_MAX_DIMS:0:-1 {
        std_FX_PTR_xD = gen_std_macro(f"FX_PTR_{i}D", 2 + i) :: std_FX_PTR_xD
        std_FX_PTR_xD_CLIP = gen_std_macro(f"FX_PTR_{i}D_CLIP", 2 + i) :: std_FX_PTR_xD_CLIP
        std_FX_PTR_xD_WRAP = gen_std_macro(f"FX_PTR_{i}D_WRAP", 2 + i) :: std_FX_PTR_xD_WRAP
        std_FX_PTR_xD_ZERO = gen_std_macro(f"FX_PTR_{i}D_ZERO", 2 + i) :: std_FX_PTR_xD_ZERO
    }
    std_fx_make_arr = gen_std_fun("fx_make_arr",
            [:: CTypCInt, make_const_ptr(CTypInt), CTypSize_t, std_CTypVoidPtr,
            std_CTypVoidPtr, std_CTypConstVoidPtr, make_ptr(std_CTypAnyArray)],
            CTypCInt)
    std_FX_ARR_SIZE = gen_std_macro("FX_ARR_SIZE", 2)
    std_FX_FREE_ARR = gen_std_macro("FX_FREE_ARR", 1)
    std_FX_MOVE_ARR = gen_std_macro("FX_MOVE_ARR", 2)
    std_fx_free_arr = gen_std_fun("fx_free_arr", [:: make_ptr(std_CTypAnyArray)], CTypVoid)
    std_fx_copy_arr = gen_std_fun("fx_copy_arr", [:: make_const_ptr(std_CTypAnyArray), make_ptr(std_CTypAnyArray)], CTypVoid)
    std_fx_copy_arr_data = gen_std_fun("fx_copy_arr_data",
        [:: make_const_ptr(std_CTypAnyArray), make_ptr(std_CTypAnyArray), CTypBool], CTypVoid)
    std_fx_subarr = gen_std_fun("fx_subarr", [:: make_const_ptr(std_CTypAnyArray), make_const_ptr(CTypInt),
                                make_ptr(std_CTypAnyArray)], CTypCInt)
    std_FX_FREE_REF_SIMPLE = gen_std_macro("FX_FREE_REF_SIMPLE", 1)
    std_fx_free_ref_simple = gen_std_fun("fx_free_ref_simple", [:: std_CTypVoidPtr], CTypVoid)
    std_FX_FREE_REF_IMPL = gen_std_macro("FX_FREE_REF_IMPL", 2)
    std_FX_MAKE_REF_IMPL = gen_std_macro("FX_MAKE_REF_IMPL", 2)
    std_FX_FREE_FP = gen_std_macro("FX_FREE_FP", 1)
    std_FX_COPY_FP = gen_std_macro("FX_COPY_FP", 2)
    std_fx_free_fp = gen_std_fun("fx_free_fp", [:: std_CTypVoidPtr], CTypVoid)
    std_fx_copy_fp = gen_std_fun("fx_copy_fp", [:: std_CTypConstVoidPtr, std_CTypVoidPtr], CTypVoid)
    std_fx_free_cptr = gen_std_fun("fx_free_cptr", [:: make_ptr(CTypCSmartPtr)], CTypVoid)
    std_fx_copy_cptr = gen_std_fun("fx_copy_cptr", [:: make_const_ptr(CTypCSmartPtr), make_ptr(CTypCSmartPtr)], CTypVoid)
    std_fx_free_vec = gen_std_fun("fx_rrb_free", [:: make_ptr(std_CTypAnyVector)], CTypVoid)
    std_fx_copy_vec = gen_std_fun("fx_rrb_copy", [:: make_const_ptr(std_CTypAnyVector), make_ptr(std_CTypAnyVector)], CTypVoid)
    std_fx_make_vec = gen_std_fun("fx_rrb_make", [:: CTypInt, CTypSize_t, std_CTypVoidPtr,
                std_CTypVoidPtr, std_CTypConstVoidPtr, make_ptr(std_CTypAnyArray)], CTypCInt)
    std_fx_ifaces_t_cptr = CTypRawPtr([:: CTypConst], CTypName(get_id("fx_ifaces_t")))
    std_FX_COPY_IFACE = gen_std_macro("FX_COPY_IFACE", 2)
    std_FX_FREE_IFACE = gen_std_macro("FX_FREE_IFACE", 1)
    std_fx_copy_iface = gen_std_fun("fx_copy_iface", [:: std_CTypConstVoidPtr, std_CTypVoidPtr], CTypVoid)
    std_fx_free_iface = gen_std_fun("fx_free_iface", [:: std_CTypVoidPtr], CTypVoid)
    std_fx_query_iface = gen_std_fun("fx_query_iface", [:: std_CTypConstVoidPtr, CTypCInt, std_CTypVoidPtr], CTypCInt)
    std_fx_get_object = gen_std_fun("fx_get_object", [:: std_CTypConstVoidPtr, CTypInt, std_CTypVoidPtr], CTypCInt)
    std_fx_make_iface = gen_std_fun("fx_make_iface", [:: std_CTypConstVoidPtr, CTypInt, std_CTypVoidPtr], CTypCInt)
}
