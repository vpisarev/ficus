open Ast
open C_form

let gen_std_fun cname argtyps rt =
    let n = gen_temp_idc cname in
    let cf = ref
    {
        cf_name=n; cf_typ=CTypFun(argtyps, rt); cf_cname=cname;
        cf_args=List.map (fun a -> noid) argtyps;
        cf_body=[];
        cf_flags=FunInC :: FunStd :: FunImpure :: [];
        cf_scope=ScGlobal :: []; cf_loc=noloc
    } in
    set_idc_entry n (CFun cf);
    n

let gen_std_macro cname nargs =
    let n = gen_temp_idc cname in
    let cm = ref
    {
        cm_name=n; cm_cname=cname; cm_args=List.init nargs (fun _ -> noid);
        cm_body=[]; cm_scope=ScGlobal :: []; cm_loc=noloc
    } in
    set_idc_entry n (CMacro cm);
    n

let init_std_names () =
    (curr_exn_val := -1024;

    std_sizeof := gen_std_fun "sizeof" (CTypAny :: []) CTypSize_t;
    std_fx_malloc := gen_std_fun "fx_malloc" (CTypSize_t :: std_CTypVoidPtr :: []) CTypCInt;
    std_fx_free := gen_std_fun "fx_free" (std_CTypVoidPtr :: []) CTypVoid;

    std_FX_CALL := gen_std_macro "FX_CALL" 2;
    std_FX_COPY_PTR := gen_std_macro "FX_COPY_PTR" 2;
    std_FX_COPY_SIMPLE := gen_std_macro "FX_COPY_SIMPLE" 2;
    std_FX_COPY_SIMPLE_BY_PTR := gen_std_macro "FX_COPY_SIMPLE_BY_PTR" 2;
    std_FX_NOP := gen_std_macro "FX_NOP" 1;
    std_FX_BREAK := gen_std_macro "FX_BREAK" 1;
    std_FX_CONTINUE := gen_std_macro "FX_CONTINUE" 1;
    std_FX_CHECK_EXN_BREAK_CONTINUE := gen_std_macro "FX_CHECK_EXN_BREAK_CONTINUE" 1;
    std_FX_CHECK_EXN_BREAK_CONTINUE_ND := gen_std_macro "FX_CHECK_EXN_BREAK_CONTINUE_ND" 2;
    std_FX_CHECK_EXN := gen_std_macro "FX_CHECK_EXN" 1;
    std_FX_CHECK_ZERO_STEP := gen_std_macro "FX_CHECK_ZERO_STEP" 2;
    std_FX_LOOP_COUNT := gen_std_macro "FX_LOOP_COUNT" 3;
    std_FX_CHECK_NE_SIZE := gen_std_macro "FX_CHECK_NE_SIZE" 2;

    std_fx_copy_ptr := gen_std_fun "fx_copy_ptr" (std_CTypConstVoidPtr :: std_CTypVoidPtr :: []) CTypVoid;

    std_FX_MAKE_STR := gen_std_macro "FX_MAKE_STR" 1;
    std_FX_FREE_STR := gen_std_macro "FX_FREE_STR" 1;
    std_fx_free_str := gen_std_fun "fx_free_str" ((make_ptr CTypString) :: []) CTypVoid;
    std_fx_copy_str := gen_std_fun "fx_copy_str" ((make_const_ptr CTypString) :: (make_ptr CTypString) :: []) CTypVoid;

    std_FX_THROW_FAST := gen_std_macro "FX_THROW_FAST" 2;
    std_FX_FREE_EXN := gen_std_macro "FX_FREE_EXN" 1;
    std_FX_COPY_EXN := gen_std_macro "FX_COPY_EXN" 2;
    std_FX_MAKE_EXN_IMPL := gen_std_macro "FX_EXN_MAKE_IMPL" 4;

    std_fx_free_exn := gen_std_fun "fx_free_exn" ((make_ptr CTypExn) :: []) CTypVoid;
    std_fx_copy_exn := gen_std_fun "fx_copy_exn" ((make_const_ptr CTypExn) :: (make_ptr CTypExn) :: []) CTypVoid;

    std_FX_FREE_LIST_SIMPLE := gen_std_macro "FX_FREE_LIST_SIMPLE" 1;
    std_fx_free_list_simple := gen_std_fun "fx_free_list_simple" (std_CTypVoidPtr :: []) CTypVoid;
    std_fx_list_length := gen_std_fun "fx_list_length" (std_CTypVoidPtr :: []) CTypInt;
    std_FX_FREE_LIST_IMPL := gen_std_macro "FX_FREE_LIST_IMPL" 2;
    std_FX_MAKE_LIST_IMPL := gen_std_macro "FX_MAKE_LIST_IMPL" 2;
    std_FX_LIST_APPEND := gen_std_macro "FX_LIST_APPEND" 3;
    std_FX_MOVE_LIST := gen_std_macro "FX_MOVE_LIST" 2;

    std_FX_CHKIDX1 := gen_std_macro "FX_CHKIDX1" 3;
    std_FX_CHKIDX := gen_std_macro "FX_CHKIDX" 2;

    std_FX_PTR_xD := [];
    for i = std_FX_MAX_DIMS downto 1 do
        std_FX_PTR_xD := (gen_std_macro (sprintf "FX_PTR_%dD" i) (2+i)) :: !std_FX_PTR_xD;
    done;

    std_fx_make_arr := gen_std_fun "fx_make_arr" [CTypCInt; (make_const_ptr CTypInt); CTypSize_t;
        std_CTypVoidPtr; std_CTypVoidPtr; std_CTypConstVoidPtr; (make_ptr std_CTypAnyArray)] CTypCInt;
    std_FX_ARR_SIZE := gen_std_macro "FX_ARR_SIZE" 2;
    std_FX_FREE_ARR := gen_std_macro "FX_FREE_ARR" 1;
    std_FX_MOVE_ARR := gen_std_macro "FX_MOVE_ARR" 2;
    std_fx_free_arr := gen_std_fun "fx_free_arr" ((make_ptr std_CTypAnyArray) :: []) CTypVoid;
    std_fx_copy_arr := gen_std_fun "fx_copy_arr"
        ((make_const_ptr std_CTypAnyArray) :: (make_ptr std_CTypAnyArray) :: []) CTypVoid;
    std_fx_subarr := gen_std_fun "fx_subarr" [(make_const_ptr std_CTypAnyArray);
        (make_const_ptr CTypInt); (make_ptr std_CTypAnyArray)] CTypCInt;

    std_FX_FREE_REF_SIMPLE := gen_std_macro "FX_FREE_REF_SIMPLE" 1;
    std_fx_free_ref_simple := gen_std_fun "fx_free_ref_simple" (std_CTypVoidPtr :: []) CTypVoid;
    std_FX_FREE_REF_IMPL := gen_std_macro "FX_FREE_REF_IMPL" 2;
    std_FX_MAKE_REF_IMPL := gen_std_macro "FX_MAKE_REF_IMPL" 2;

    std_FX_FREE_FP := gen_std_macro "FX_FREE_FP" 1;
    std_FX_COPY_FP := gen_std_macro "FX_COPY_FP" 2;
    std_fx_free_fp := gen_std_fun "fx_free_fp" (std_CTypVoidPtr :: []) CTypVoid;
    std_fx_copy_fp := gen_std_fun "fx_copy_fp" (std_CTypConstVoidPtr :: std_CTypVoidPtr :: []) CTypVoid;

    std_fx_free_cptr := gen_std_fun "fx_free_cptr" ((make_ptr CTypCSmartPtr) :: []) CTypVoid;
    std_fx_copy_cptr := gen_std_fun "fx_copy_cptr"
        ((make_const_ptr CTypCSmartPtr) :: (make_ptr CTypCSmartPtr) :: []) CTypVoid)
