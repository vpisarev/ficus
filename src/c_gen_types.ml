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

(* converts function arguments and the return type to ctyp_t.
   where needed, adds pointers. The return value is added to the argument list.
   For example,
   (int, float [,], string list) -> int
   is converted to
   ( int*, fx_arr_t*, _fx_LS, int* )
*)
let rec ktyp2ctyp_fargs args rt loc =
    let args_ = List.map (fun kt ->
        let ct = ktyp2ctyp kt loc in
        let {ctp_pass_by_ref} = get_ctprops ct loc in
        let ptr_attr = match (ktyp_deref kt loc) with
                | KTypArray(_, _) -> []
                | _ -> [CTypConst] in
        if ctp_pass_by_ref then CTypRawPtr(ptr_attr, ct) else ct) args in
    let rct = CTypRawPtr([], ktyp2ctyp rt loc) in
    args_ @ (rct :: [])

(* ktyp_t -> ctyp_t *)
and ktyp2ctyp t loc =
    let report_err tname =
        raise_compile_err loc (sprintf "ktyp2ctyp: %s is not supported here. Should be converted to KTypName()" tname)
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
        | KTypCPointer -> CTypCSmartPtr
        | KTypFun (args, rt) ->
            let args_ = ktyp2ctyp_fargs args rt loc in
            CTypFun(args_, CTypInt)
        | KTypTuple _ -> report_err "KTypTuple"
        | KTypRecord _ -> report_err "KTypRecord"
        | KTypName i -> CTypName i
        | KTypArray (d, et) -> CTypArray(d, ktyp2ctyp_ et)
        | KTypList _ -> report_err "KTypList"
        | KTypRef _ -> report_err "KTypRef"
        | KTypExn -> CTypExn
        | KTypErr -> CTypVoid
        | KTypModule -> CTypVoid
    in ktyp2ctyp_ t

(* returns some basic information about ctyp_t instance *)
and get_ctprops ct loc =
    match ct with
    | CTypInt | CTypCInt | CTypSize_t | CTypSInt _ | CTypUInt _
    | CTypFloat _ | CTypVoid | CTypNil | CTypBool | CTypWChar ->
        {ctp_free=(noid, noid); ctp_copy=(noid, noid);
        ctp_pass_by_ref=false; ctp_ptr=false}
    | CTypCSmartPtr ->
        {ctp_free=(noid, !std_fx_free_cptr); ctp_copy=(noid, !std_fx_copy_cptr);
        ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypString ->
        {ctp_free=(noid, !std_fx_free_str);
        ctp_copy=(noid, !std_fx_copy_str); ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypExn ->
        {ctp_free=(noid, !std_fx_free_exn); ctp_copy=(noid, !std_fx_copy_exn);
        ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypStruct(Some(i), _) -> get_ctprops (CTypName i) loc
    | CTypStruct _ -> raise_compile_err loc "there is no type properties for the anonymoous struct"
    | CTypUnion(Some(i), _) ->
        raise_compile_err loc (sprintf "there is no type properties for union '%s'" (id2str i))
    | CTypUnion _ -> raise_compile_err loc "there is no type properties for the anonymoous union"
    | CTypFun(_, _) ->
        {ctp_free=(!std_FX_FREE_FP, !std_fx_free_fp);
        ctp_copy=(!std_FX_COPY_FP, !std_fx_copy_fp);
        ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypFunRawPtr(_, _) ->
        {ctp_free=(noid, noid); ctp_copy=(noid, noid);
        ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypRawPtr(_, t) ->
        {ctp_free=(noid, noid); ctp_copy=(noid, noid);
        ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypArray(_, _) ->
        {ctp_free=(noid, !std_fx_free_fp); ctp_copy=(noid, !std_fx_copy_arr);
        ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypName i ->
        (match (cinfo i) with
        | CTyp {contents={ct_props}} -> ct_props
        | _ -> raise_compile_err loc
            (sprintf "properties of non-type '%s' cannot be requested" (id2str i)))
    | CTypLabel ->
        raise_compile_err loc "properties of label cannot be requested"
    | CTypAny ->
        raise_compile_err loc "properties of 'any type' cannot be requested"

let get_free_f ct let_none loc =
    let {ctp_free=(freem, freef)} = get_ctprops ct loc in
    if freem = noid && freef = noid && let_none then
        None
    else
        let i = if freem <> noid then freem
            else if freef <> noid then freef
            else !std_FX_NOP in
        Some (CExpIdent(i, ((get_idc_typ i loc), loc)))

let get_copy_f ct let_none loc =
    let {ctp_pass_by_ref; ctp_copy=(copym, copyf)} = get_ctprops ct loc in
    (ctp_pass_by_ref,
    (if copym = noid && copyf = noid && let_none then
        None
    else
        let i = if copym <> noid then copym
            else if copyf <> noid then copyf
            else if ctp_pass_by_ref then !std_FX_COPY_SIMPLE_BY_PTR
            else !std_FX_COPY_SIMPLE in
        Some (CExpIdent(i, ((get_idc_typ i loc), loc)))))

(* generates the copy/assignment expression.
   It's assumed that the destination is empty, i.e. not initialized.
   If it's initialized then the compiler should call the destructor for it first *)
let gen_copy_code src_exp dst_exp ct code loc =
    let (pass_by_ref, copy_f_opt) = get_copy_f ct true loc in
    let ctx = (CTypVoid, loc) in
    let dst_exp = CExpUnOp(COpGetAddr, dst_exp, ((make_ptr ct), loc)) in
    let src_exp = if pass_by_ref then CExpUnOp(COpGetAddr, src_exp, ((make_ptr ct), loc)) else src_exp in
    let e = match copy_f_opt with
            | Some(f) -> CExpCall(f, src_exp :: dst_exp :: [], ctx)
            | _ -> (* in C the assignment operator returns assigned value,
                    but since in this particular case we are not going to chain
                    assignment operators, we assume that it returns 'void' *)
                    CExpBinOp(COpAssign, dst_exp, src_exp, ctx)
    in (CExp e) :: code

(* generates the destructor call if needed *)
let gen_free_code elem_exp ct code loc =
    let free_f_opt = get_free_f ct true loc in
    let ctx = (CTypVoid, loc) in
    let elem_exp = CExpUnOp(COpGetAddr, elem_exp, ((make_ptr ct), loc)) in
    match free_f_opt with
    | Some(f) -> CExp(CExpCall(f, elem_exp :: [], ctx)) :: code
    | _ -> code

let convert_all_typs top_code =
    let top_fwd_decl = ref ([]: cstmt_t list) in
    let top_typ_decl = ref ([]: cstmt_t list) in
    let all_decls = ref (IdSet.empty) in
    let all_fwd_decls = ref (IdSet.empty) in
    let all_visited = ref (IdSet.empty) in

    let add_fwd_decl fwd_decl decl =
        if fwd_decl then
            top_fwd_decl := decl :: !top_fwd_decl
        else () in
    let add_decl decl =
        top_typ_decl := decl :: !top_typ_decl in
    (* creates declaration of the data structure with optional forward declaration;
       if needed, create declarations (empty at this momoent) of the destructor and
       copy operator, together with thier optional forward declarations *)
    let create_ctyp_decl tn ktp fwd_decl loc =
        let {ktp_ptr; ktp_pass_by_ref; ktp_custom_free; ktp_custom_copy} = ktp in
        let (freem, freef) = if ktp_custom_free then (noid, (gen_temp_idc "free")) else (noid, noid) in
        let (cpym, copyf) = if ktp_custom_copy then (noid, (gen_temp_idc "copy")) else
            if ktp_ptr then (!std_FX_COPY_PTR, !std_fx_copy_ptr) else (noid, noid) in
        let cname = get_idk_cname tn loc in
        let (struct_id, struct_cname) = if ktp_ptr then ((dup_idc tn), cname ^ "_data_t") else (tn, cname) in
        let struct_decl = ref { ct_name=struct_id;
            ct_typ=CTypStruct((Some struct_id), []);
            ct_ktyp=KTypName tn; ct_cname=struct_cname;
            ct_make=noid;  ct_scope=ScGlobal::[]; ct_loc=loc;
            ct_props={
                ctp_ptr=ktp_ptr; ctp_pass_by_ref=ktp_pass_by_ref;
                ctp_free=(freem, freef); ctp_copy=(cpym, copyf)}
            } in
        let typ_decl = if ktp_ptr then
            ref { !struct_decl with ct_name=tn; ct_typ=(make_ptr (CTypName struct_id)); ct_cname=cname }
            else struct_decl in
        let ctyp = CTypName struct_id in
        let (src_typ, dst_typ0) = ((make_const_ptr ctyp), (make_ptr ctyp)) in
        let dst_typ = if ktp_ptr then (make_ptr dst_typ0) else dst_typ0 in
        let src_id = get_id "src" in
        let dst_id = get_id "dst" in
        let src_exp = CExpIdent(src_id, (src_typ, loc)) in
        let dst_exp = CExpIdent(dst_id, (dst_typ, loc)) in
        let dst_exp = if ktp_ptr then CExpUnOp(COpDeref, dst_exp, (dst_typ0, loc)) else dst_exp in
        let freef_decl = ref {
            cf_name=freef; cf_typ=CTypFun(dst_typ :: [], CTypVoid); cf_cname="_fx_free_" ^ cname;
            cf_args=dst_id :: [];  cf_body=[];
            cf_flags=FunNoThrow :: []; cf_scope=ScGlobal :: []; cf_loc=loc } in
        let copyf_decl = ref { !freef_decl with
            cf_name=freef; cf_typ=CTypFun(src_typ :: dst_typ :: [], CTypVoid); cf_cname="_fx_copy_" ^ cname;
            cf_args=src_id :: dst_id :: [] } in
        if ktp_ptr then
            (set_idc_entry struct_id (CTyp struct_decl);
            add_fwd_decl fwd_decl (CDefForwardTyp (struct_id, loc)))
        else ();
        set_idc_entry tn (CTyp typ_decl);
        add_decl (CDefTyp struct_decl);
        if ktp_custom_free then
            (set_idc_entry freef (CFun freef_decl);
            add_fwd_decl fwd_decl (CDefForwardFun (freef, loc));
            add_decl (CDefFun freef_decl))
        else ();
        if ktp_custom_copy then
            (set_idc_entry copyf (CFun copyf_decl);
            add_fwd_decl fwd_decl (CDefForwardFun (copyf, loc));
            add_decl (CDefFun copyf_decl))
        else ();
        (struct_decl, freef_decl, copyf_decl, src_exp, dst_exp)
    in
    (* converts named type to C *)
    let rec cvt2ctyp tn loc =
        if (IdSet.mem tn !all_decls) then ()
        else
            let visited = IdSet.mem tn !all_visited in
            let deps = K_annotate_types.get_typ_deps tn loc in
            let kt_info = kinfo_ tn loc in
            let (opt_rec_var, fwd_decl, deps) = match kt_info with
                | KVariant kvar ->
                    let {kvar_flags} = !kvar in
                    if (List.mem VariantRecursive kvar_flags) then
                        let fwd_decl = not (IdSet.mem tn !all_fwd_decls) in
                        ((Some kvar), fwd_decl, (IdSet.filter (fun d -> d != tn) deps))
                    else
                        (None, false, deps)
                | _ -> (None, false, deps) in
            let _ = match (opt_rec_var, visited) with
                | (None, true) -> raise_compile_err loc
                    (sprintf "non-recursive variant type '%s' references itself directly or indirectly" (id2str tn))
                | _ -> () in
            let _ = all_visited := IdSet.add tn !all_visited in
            let ktp = K_annotate_types.get_ktprops (KTypName tn) loc in
            let (struct_decl, freef_decl, copyf_decl, src_exp, dst_exp) = create_ctyp_decl tn ktp fwd_decl loc in
            let {ct_name=struct_id} = !struct_decl in
            IdSet.iter (fun dep -> let dep_loc = get_idk_loc dep in cvt2ctyp dep dep_loc) deps;
            all_decls := IdSet.add tn !all_decls;
            match kt_info with
            | KTyp kt ->
                let {kt_typ; kt_loc} = !kt in
                (match kt_typ with
                | KTypTuple(telems) ->
                    let (_, free_code, copy_code, relems) =
                        List.fold_left (fun (i, free_code, copy_code, relems) kt ->
                            let ni = get_id (sprintf "t%i" i) in
                            let ti = ktyp2ctyp kt kt_loc in
                            let selem = CExpArrow(src_exp, ni, (ti, loc)) in
                            let delem = CExpArrow(dst_exp, ni, (ti, loc)) in
                            let free_code = gen_free_code delem ti free_code kt_loc in
                            let copy_code = gen_copy_code selem delem ti copy_code kt_loc in
                            (i+1, free_code, copy_code, (ni, ti) :: relems)) (0, [], [], []) telems in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct((Some struct_id), List.rev relems)};
                    freef_decl := {!freef_decl with cf_body=List.rev free_code};
                    copyf_decl := {!freef_decl with cf_body=List.rev copy_code}
                | KTypRecord(_, relems) ->
                    let (free_code, copy_code, relems) =
                        List.fold_left (fun (free_code, copy_code, relems) (ni, kt) ->
                            let ti = ktyp2ctyp kt loc in
                            let selem = CExpArrow(src_exp, ni, (ti, kt_loc)) in
                            let delem = CExpArrow(dst_exp, ni, (ti, kt_loc)) in
                            let free_code = gen_free_code delem ti free_code kt_loc in
                            let copy_code = gen_copy_code selem delem ti copy_code kt_loc in
                            (free_code, copy_code, (ni, ti) :: relems)) ([], [], []) relems in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct((Some struct_id), List.rev relems)};
                    freef_decl := {!freef_decl with cf_body=List.rev free_code};
                    copyf_decl := {!freef_decl with cf_body=List.rev copy_code}
                | KTypList et ->
                    let ct = ktyp2ctyp et kt_loc in
                    let relems = ((get_id "rc"), CTypInt) ::
                        ((get_id "tl"), (make_ptr (CTypName struct_id))) ::
                        ((get_id "hd"), ct) :: [] in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct((Some struct_id), relems)}
                | KTypRef(et) -> ()
                | _ -> ())
            | KVariant kv -> ()
            | _ -> raise_compile_err loc (sprintf "type '%s' cannot be converted to C" (id2str tn))
    in
    let rec fold_n_cvt_ktyp t loc callb = ()
    and fold_n_cvt_kexp e callb =
        match e with
        | KDefTyp {contents={kt_name; kt_loc}} -> cvt2ctyp kt_name kt_loc
        | KDefVariant {contents={kvar_name; kvar_loc}} -> cvt2ctyp kvar_name kvar_loc
        | KDefClosureVars {contents={kcv_name; kcv_loc}} -> cvt2ctyp kcv_name kcv_loc
        | KDefExn {contents={ke_name; ke_loc}} -> () (* [TODO] convert exception to C *)
        | _ -> fold_kexp e callb
            (*
                Tuples:
                ///////// simple tuples /////////
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
    in let fold_n_cvt_callb =
    {
        kcb_fold_ktyp = Some(fold_n_cvt_ktyp);
        kcb_fold_kexp = Some(fold_n_cvt_kexp);
        kcb_fold_atom = None;
        kcb_fold_result = 0
    }
    in List.iter (fun e -> fold_n_cvt_kexp e fold_n_cvt_callb) top_code;
    (List.rev !top_fwd_decl) @ (List.rev !top_typ_decl)

let convert_to_c top_code =
    let ccode = convert_all_typs top_code in
    ccode
