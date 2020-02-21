(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Generate C code (in the tree-like representation, see C_form.ml)
    out of K-form (after the lambda lifting step and possibly other
    similar lowering transformations).
*)

(*
    The algorithm:
    1. traverse through the code; represent each complex type of K-form is a C structure;
      earlier on (in k_mangle.ml), we replaced all the complex types with KTypName(...)
      and gave them unique names. now need to convert those types to C,
      put all the definitions in the beginning of the generated C code.
      If a type definition depends on other types that are delcared later
      (e.g. in the case of mutually-recursive variants), insert forward declarations
      of those types ('struct _fx_V<dep_variant_name>;')

    2. traverse through the code:
        For each function:
        * form declaration of each function in C:
          * add the context parameter and the closure parameter
          * make the return value an output parameter; make "int" the actual return parameter
        * collect information about the local values/variables
          * find out the C type of each local value
            ** TEMP_REF values should become pointers!
          * find which local values need cleanup; their declarations should be
            put in the beginning of the function with "nil" initializer
          * convert the code step by step with a special treatment for try-catch, loops and especially KExpMap.
            Each function call (unless there is no-throw flag) needs to be wrapped in a macro that checks for errors.
*)

open Ast
open K_form
open C_form

let ktyp2ctyp t loc =
    let report_err tname =
        raise_compile_err loc (sprintf "%s is not supported here. Should be converted to KTypName()" tname)
    in
    let rec ktyp2ctyp_ t =
        match t with
        | KTypInt -> CTypInt
        | KTypSInt n -> CTypSInt n
        | KTypUInt n -> CTypUInt n
        | KTypFloat n -> CTypFloat n
        | KTypVoid -> CTypVoid
        | KTypNil -> CTypNil
        | KTypBool -> CTypBool
        | KTypChar -> CTypWChar
        | KTypString -> CTypString
        | KTypCPointer -> CTypCSmartPointer
        | KTypFun (args, rt) -> CTypFunPtr((List.map ktyp2ctyp_ args), ktyp2ctyp_ rt)
        | KTypTuple _ -> report_err "KTypTuple"
        | KTypRecord _ -> report_err "KTypRecord"
        | KTypName i -> CTypName i
        | KTypArray _ -> report_err "KTypArray"
        | KTypList _ -> report_err "KTypList"
        | KTypRef _ -> report_err "KTypRef"
        | KTypExn -> CTypExn
        | KTypErr -> CTypVoid
        | KTypModule -> CTypVoid
    in ktyp2ctyp_ t

let convert_all_typs top_code =
    let top_fwd_decl = ref ([]: cstmt_t list) in
    let top_typ_decl = ref ([]: cstmt_t list) in
    let top_typ_ops = ref ([]: cstmt_t list) in
    let all_decls = ref (IdSet.empty) in
    let all_fwd_decls = ref (IdSet.empty) in
    let all_visited = ref (IdSet.empty) in

    let add_fwd_decl fwd_decl decl =
        if fwd_decl then
            top_fwd_decl := decl :: !top_fwd_decl
        else ()
    in
    let create_ctyp_decl tn ptr_typ gen_free gen_cpy fwd_decl loc =
        let is_simple = gen_free || gen_cpy in
        let freef = if gen_free then (gen_temp_idc "free") else noid in
        let cpyf = if gen_cpy then (gen_temp_idc "copy") else noid in
        let cname = get_id_cname tn loc in
        let (struct_id, struct_cname) = if ptr_typ then ((dup_idc tn), cname ^ "_data_t") else (tn, cname) in
        let kind = (if is_simple then CKindSimple else CKindStruct) in
        let struct_decl = ref { ct_name=struct_id;
            ct_typ=CTypStruct((Some struct_id), []);
            ct_ktyp=KTypName tn; ct_cname=struct_cname;
            ct_kind=kind; ct_flags=[]; ct_make=noid; ct_free=freef; ct_copy=cpyf;
            ct_scope=ScGlobal::[]; ct_loc=loc } in
        let typ_decl = if ptr_typ then
            ref { !struct_decl with ct_name=tn; ct_typ=(make_ptr struct_id); ct_cname=cname }
            else struct_decl in
        let ctyp = CTypName struct_id in
        let (src_typ, dst_typ) = ((make_const_ptr ctyp), (make_ptr ctyp)) in
        let dst_typ = if ptr_typ then (make_ptr dst_typ) else dst_typ in
        let freef_decl = ref {
            cf_name=freef; cf_typ=CTypFun(dst_typ :: [], CTypVoid); cf_cname="_fx_free_" ^ cname;
            cf_args=(get_id "dst") :: [];  cf_body=CStmtNop(loc);
            cf_flags=FunNoThrow :: []; cf_scope=ScGlobal :: []; cf_loc=loc } in
        let cpyf_decl = ref { !freef_decl with
            cf_name=freef; cf_typ=CTypFun(src_typ :: dst_typ :: [], CTypVoid); cf_cname="_fx_copy_" ^ cname;
            cf_args=(get_id "src") :: (get_id "dst") :: [] } in
        if ptr_typ then
            set_idc_entry struct_id (CTyp struct_decl);
            add_fwd_decl fwd_decl (CDefForwardTyp struct_id)
        else ();
        set_idc_entry tn (CTyp typ_decl);
        if gen_free then
            set_idc_entry freef (CFun freef_decl);
            add_fwd_decl fwd_decl (CDefForwardFun freef)
        else ();
        if gen_cpy then
            set_idc_entry cpyf (CFun cpyf_decl);
            add_fwd_decl fwd_decl (CDefForwardFun cpyf)
        else ();
        (tn, struct_id, freef, cpyf)

    let rec cvt2ctyp tn loc =
        if (IdSet.mem tn !all_decls) then ()
        else
            let visited = IdSet.mem tn !all_visited in
            let deps = K_annotate_types.get_typ_deps tn loc in
            let (opt_rec_var, opt_rec_cvar, deps) = match (kinfo_ tn loc) with
                | KVariant kvar ->
                    let {kvar_flags} = !kvar in
                    if (List.mem VariantRecursive kvar_flags) then
                        (if (IdSet.mem tn !all_fwd_decls) then ()
                        else
                        ((Some kvar), (List.filter (fun d -> d != tn) deps)))
                    else
                        (None, None, deps)
                | _ -> (None, None, deps)
            let _ = match (opt_rec_var, visited) with
                | (None, true) -> raise_compile_err loc
                    (sprintf "")
                | _ -> ()
            let _ = all_visited := IdSet.add tn !all_visited in

    let rec fold_n_cvt_ktyp t loc callb = ()
    and fold_n_cvt_kexp e loc callb =
        match e with
        | KDefGenTyp {contents={kgen_name; kgen_loc}} -> cvt2ctyp kgen_name kgen_loc
        | KDefRecord {contents={krec_name; krec_loc}} -> cvt2ctyp krec_name krec_loc
        | KDefVariant {contents={kvar_name; kvar_loc}} -> cvt2ctyp kvar_name kvar_loc
        | KDefClosureVars {contents={kcv_name; kcv_loc}} -> cvt2ctyp kcv_name kcv_loc
        | KDefExn {contents={ke_name; ke_loc}} -> cvt2ctyp ke_name ke_loc
        | _ -> fold_kexp e callb
            (*
                Tuples:
                //////// simple tuples /////////
                typedef struct _fx_Ta3f
                {
                    float t0;
                    float t1;
                    float t2;
                } _fx_Ta3f;

                // instead of _fx_make_Ta3f() we use
                _fx_Ta3f t = {t0_val, t1_val, t2_val};
                // we do not need destructor, to copy a simple tuple we use '='

                /////// complex tuples ///////
                typedef struct _fx_T2iS
                {
                    int_ t0;
                    fx_str_t t1;
                } _fx_T2iS;

                FX_INLINE void _fx_free_T2iS(_fx_T2iS* t)
                {
                    // call destructor for all non-trivial members
                    fx_free_str(&t->t1);
                }

                //
                FX_INLINE void _fx_make_T2iS( int_ t0, const fx_str_t* t1, _fx_T2iS* fx_result )
                {
                    fx_result->t0 = t0;
                    fx_copy_str(t1, &fx_result->t1);
                }

                FX_INLINE void _fx_copy_T2iS( const _fx_T2iS* src, _fx_T2iS* dst)
                {
                    _fx_make_T2iS(src->t0, &src->t1, dst);
                }
            *)

            (*
                // simple exceptions
                #define _FX_EXN_MyException -500
                FX_INLINE int _fx_make_E11MyException(fx_exn_t* fx_result)
                {
                    fx_result->tag = _FX_EXN_MyException;
                    fx_result->data = 0;
                    return FX_OK;
                }
                // or simply fx_exn_t exn = {_FX_EXN_MyException, 0};

                // complex exceptions
                #define _FX_EXN_Failwith -501
                typedef struct _fx_E8Failwith_data_t
                {
                    fx_exndata_t base; // contains refcount and destructor ptr
                    fx_str_t arg;
                } _fx_E8Failwith_data_t;

                void _fx_free_E8Failwith(fx_exndata_t* data_)
                {
                    _fx_E8Failwith_data_t* data = (_fx_E8Failwith_data_t* )data_;
                    fx_free_str(&data->arg);
                    fx_free(data;
                }

                int _fx_make_E8Failwith(const fx_str_t* arg, fx_exn_t* fx_result)
                {
                    FX_EXN_MAKE_IMPL(_FX_EXN_Failwith, _fx_E8Failwith_data_t, _fx_free_E8Failwith,
                                    fx_copy_str(arg, &data->arg));
                }

                #define _FX_THROW_E8Failwith(arg, catch_label) \
                    fx_status = _fx_make_E8Failwith(arg, &fx_ctx->exn, false); \
                    goto catch_label
            *)


let convert_to_c top_code =
    let ccode = convert_all_typs top_code in
    ccode
