(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Generates C code for the data structures and the supporting
    functions (_fx_free_..., _fx_copy_..., _fx_make_... etc.).

    We traverse through the code; represent each complex type of K-form is a C structure;
    earlier on (in k_mangle.ml), we replaced all the complex types with KTypName(...)
    and gave them unique names. now need to convert those types to C,
    put all the definitions in the beginning of the generated C code.
    If a type definition depends on other types that are delcared later
    (e.g. in the case of mutually-recursive variants), insert forward declarations
    of those types ('struct _fx_V<dep_variant_name>;') and the forward declarations of
    their destructors.
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
        let ctyp = ktyp2ctyp kt loc in
        let {ktp_pass_by_ref} = K_annotate_types.get_ktprops kt loc in
        let ptr_attr = match (deref_ktyp kt loc) with
                | KTypArray(_, _) -> []
                | _ -> [CTypConst] in
        if ktp_pass_by_ref then CTypRawPtr(ptr_attr, ctyp) else ctyp) args in
    let rct = if rt = KTypVoid then [] else [CTypRawPtr([], ktyp2ctyp rt loc)] in
    args_ @ rct @ [std_CTypVoidPtr]

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
        | KTypNil -> CTypRawPtr([], CTypVoid)
        | KTypBool -> CTypBool
        | KTypChar -> CTypUniChar
        | KTypString -> CTypString
        | KTypCPointer -> CTypCSmartPtr
        | KTypFun (args, rt) ->
            let args_ = ktyp2ctyp_fargs args rt loc in
            CTypFun(args_, CTypCInt)
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
and get_ctprops ctyp loc =
    let ctp =
    (match ctyp with
    | CTypInt | CTypCInt | CTypSize_t | CTypSInt _ | CTypUInt _
    | CTypFloat _ | CTypVoid | CTypBool | CTypUniChar ->
        {ctp_scalar=true; ctp_complex=false; ctp_make=[]; ctp_free=(noid, noid); ctp_copy=(noid, noid);
        ctp_pass_by_ref=false; ctp_ptr=false}
    | CTypCSmartPtr ->
        {ctp_scalar=false; ctp_complex=true; ctp_make=[];
        ctp_free=(noid, !std_fx_free_cptr); ctp_copy=(noid, !std_fx_copy_cptr);
        ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypString ->
        {ctp_scalar=false; ctp_complex=true; ctp_make=[]; ctp_free=(!std_FX_FREE_STR, !std_fx_free_str);
        ctp_copy=(noid, !std_fx_copy_str); ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypExn ->
        {ctp_scalar=false; ctp_complex=true; ctp_make=[]; ctp_free=(noid, !std_fx_free_exn);
        ctp_copy=(noid, !std_fx_copy_exn); ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypStruct(Some(i), _) -> get_ctprops (CTypName i) loc
    | CTypStruct _ -> raise_compile_err loc "there is no type properties for the anonymoous struct"
    | CTypUnion(Some(i), _) ->
        raise_compile_err loc (sprintf "there is no type properties for union '%s'" (id2str i))
    | CTypUnion _ -> raise_compile_err loc "there is no type properties for the anonymoous union"
    | CTypFun(_, _) ->
        {ctp_scalar=false; ctp_complex=true; ctp_make=[]; ctp_free=(!std_FX_FREE_FP, !std_fx_free_fp);
        ctp_copy=(!std_FX_COPY_FP, !std_fx_copy_fp); ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypFunRawPtr(_, _) ->
        {ctp_scalar=true; ctp_complex=false; ctp_make=[]; ctp_free=(noid, noid);
        ctp_copy=(noid, noid); ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypRawPtr(_, t) ->
        {ctp_scalar=true; ctp_complex=false; ctp_make=[]; ctp_free=(noid, noid);
        ctp_copy=(noid, noid); ctp_pass_by_ref=false; ctp_ptr=true}
    | CTypRawArray(_, t) ->
        {ctp_scalar=false; ctp_complex=false; ctp_make=[]; ctp_free=(noid, noid);
        ctp_copy=(noid, noid); ctp_pass_by_ref=false; ctp_ptr=false}
    | CTypArray(_, _) ->
        {ctp_scalar=false; ctp_complex=true; ctp_make=[]; ctp_free=(!std_FX_FREE_ARR, !std_fx_free_arr);
        ctp_copy=(noid, !std_fx_copy_arr); ctp_pass_by_ref=true; ctp_ptr=false}
    | CTypName i ->
        (match (cinfo_ i loc) with
        | CTyp {contents={ct_props}} -> ct_props
        | _ -> raise_compile_err loc
            (sprintf "properties of non-type '%s' cannot be requested" (id2str i)))
    | CTypLabel ->
        raise_compile_err loc "properties of label cannot be requested"
    | CTypAny ->
        raise_compile_err loc "properties of 'any type' cannot be requested") in
    let {ctp_free=(free_m, free_f); ctp_copy=(copy_m, copy_f)} = ctp in
    (if free_m != noid && free_f = noid then raise_compile_err loc "cgen: if free_m is non-empty then free_f must also be non-empty" else ();
    if copy_m != noid && copy_f = noid then raise_compile_err loc "cgen: if copy_m is non-empty then copy_f must also be non-empty" else ();
    ctp)

let get_constructor ctyp required loc =
    let {ctp_make} = get_ctprops ctyp loc in
    match ctp_make with
    | i :: [] -> i
    | _ -> if required then raise_compile_err loc "cgen: missing type constructor" else noid

let get_free_f ctyp let_none let_macro loc =
    let {ctp_ptr; ctp_free=(free_m, free_f)} = get_ctprops ctyp loc in
    if free_m = noid && free_f = noid && let_none then
        (false, None)
    else
        let (use_if, i) = if let_macro && free_m <> noid then (false, free_m)
            else if free_f <> noid then (ctp_ptr, free_f)
            else (false, !std_FX_NOP) in
        (use_if, Some (make_id_exp i loc))

let get_copy_f ctyp let_none let_macro loc =
    let {ctp_pass_by_ref; ctp_copy=(copy_m, copy_f)} = get_ctprops ctyp loc in
    (ctp_pass_by_ref,
    (if copy_m = noid && copy_f = noid && let_none then
        None
    else
        let i = if copy_m <> noid && let_macro then copy_m
            else if copy_f <> noid then copy_f
            else if ctp_pass_by_ref then !std_FX_COPY_SIMPLE_BY_PTR
            else !std_FX_COPY_SIMPLE in
        Some (make_id_exp i loc)))

(* generates the copy/assignment expression.
   It's assumed that the destination is empty, i.e. not initialized.
   If it's initialized then the compiler should call the destructor for it first *)
let gen_copy_code src_exp dst_exp ctyp code loc =
    let (pass_by_ref, copy_f_opt) = get_copy_f ctyp true true loc in
    let ctx = (CTypVoid, loc) in
    let e = match copy_f_opt with
        | Some(f) ->
            let src_exp = if pass_by_ref then (cexp_get_addr src_exp) else src_exp in
            let dst_exp = cexp_get_addr dst_exp in
            CExpCall(f, src_exp :: dst_exp :: [], ctx)
        | _ -> (* in C the assignment operator returns assigned value,
                but since in this particular case we are not going to chain
                assignment operators, we assume that it returns 'void' *)
                CExpBinOp(COpAssign, dst_exp, src_exp, ctx)
    in (CExp e) :: code

(* generates the destructor call if needed *)
let gen_free_code elem_exp ctyp let_macro use_if code loc =
    let (can_use_if, free_f_opt) = get_free_f ctyp true let_macro loc in
    let ctx = (CTypVoid, loc) in
    let elem_exp_ptr = cexp_get_addr elem_exp in
    match free_f_opt with
    | Some(f) ->
        let call_stmt = CExp(CExpCall(f, elem_exp_ptr :: [], ctx)) in
        let stmt = if can_use_if && use_if then
            CStmtIf(elem_exp, call_stmt, (CStmtNop loc), loc) else call_stmt in
        stmt :: code
    | _ -> code

type ctyp_temp_info_t =
{
    ctti_struct_decl: cdeftyp_t ref;
    ctti_freef_decl: cdeffun_t ref;
    ctti_copyf_decl: cdeffun_t ref;
    ctti_src_exp: cexp_t;
    ctti_dst_exp: cexp_t;
    ctti_cname_wo_prefix: string
}

let convert_all_typs top_code =
    let top_fwd_decl = ref ([]: cstmt_t list) in
    let top_typ_decl = ref ([]: cstmt_t list) in
    let top_typfun_decl = ref ([]: cstmt_t list) in
    let all_decls = ref (IdSet.empty) in
    let all_fwd_decls = ref (IdSet.empty) in
    let all_visited = ref (IdSet.empty) in
    let all_var_enums = ref (Env.empty : id_t Env.t) in
    let all_saved_rec_vars = ref (Env.empty: ctyp_temp_info_t Env.t) in

    let add_fwd_decl i fwd_decl decl =
        if i = noid then ()
        else
            let have_fwd_decl = IdSet.mem i !all_fwd_decls in
            if (not have_fwd_decl) && fwd_decl then
                (all_fwd_decls := IdSet.add i !all_fwd_decls;
                top_fwd_decl := decl :: !top_fwd_decl)
            else ()
    in
    let add_decl i decl =
        if i = noid then ()
        else
            let _ = all_decls := IdSet.add i !all_decls in
            match decl with
            | CDefFun _ ->
                top_typfun_decl := decl :: !top_typfun_decl
            | _ ->
                top_typ_decl := decl :: !top_typ_decl
    in
    (* creates declaration of the data structure with optional forward declaration;
       if needed, creates declarations of the destructor and
       copy operator (with empty bodies; the bodies are updated later on),
       together with thier optional forward declarations *)
    let create_ctyp_decl tn fwd_decl loc =
        let ktp = K_annotate_types.get_ktprops (KTypName tn) loc in
        let {ktp_ptr; ktp_pass_by_ref; ktp_custom_free; ktp_custom_copy} = ktp in
        let (free_m, free_f) = if ktp_custom_free then (noid, (gen_temp_idc "free")) else (noid, noid) in
        let (copy_m, copy_f) = if ktp_custom_copy then (noid, (gen_temp_idc "copy")) else
            if ktp_ptr then (!std_FX_COPY_PTR, !std_fx_copy_ptr) else (noid, noid) in
        let cname = get_idk_cname tn loc in
        let struct_id = if ktp_ptr then get_id (cname ^ "_data_t") else tn in
        let struct_typ = CTypStruct((Some struct_id), []) in
        let struct_or_ptr_typ = if ktp_ptr then (make_ptr struct_typ) else struct_typ in
        let struct_decl = ref { ct_name=tn; ct_typ = struct_or_ptr_typ;
            ct_ktyp=KTypName tn; ct_cname=cname; ct_tagenum=noid; ct_data_start=0;
            ct_scope=ScGlobal::[]; ct_loc=loc;
            ct_props={
                ctp_scalar=false; ctp_complex=ktp_custom_free || ktp_custom_copy;
                ctp_ptr=ktp_ptr; ctp_pass_by_ref=ktp_pass_by_ref;
                ctp_make=[]; ctp_free=(free_m, free_f); ctp_copy=(copy_m, copy_f)
                }
            } in
        let ctyp = CTypName tn in
        let (src_typ0, dst_typ) = (ctyp, (make_ptr ctyp)) in
        let src_typ = if ktp_ptr then src_typ0 else (make_const_ptr src_typ0) in
        let src_id = get_id "src" in
        let dst_id = get_id "dst" in
        let src_exp = CExpIdent(src_id, (src_typ, loc)) in
        let dst_exp = CExpIdent(dst_id, (dst_typ, loc)) in
        let cname_wo_prefix = K_mangle.remove_fx cname in
        let freef_decl = ref {
            cf_name=free_f; cf_typ=CTypFun(dst_typ :: [], CTypVoid); cf_cname="_fx_free_" ^ cname_wo_prefix;
            cf_args=dst_id :: [];  cf_body=[];
            cf_flags=FunNoThrow :: []; cf_scope=ScGlobal :: []; cf_loc=loc } in
        let copyf_decl = ref { !freef_decl with
            cf_name=copy_f; cf_typ=CTypFun(src_typ :: dst_typ :: [], CTypVoid); cf_cname="_fx_copy_" ^ cname_wo_prefix;
            cf_args=src_id :: dst_id :: [] } in
        set_idc_entry tn (CTyp struct_decl);
        if not ktp_ptr then () else
            add_fwd_decl tn fwd_decl (CDefForwardTyp (struct_id, loc));
        if ktp_custom_free then
            (set_idc_entry free_f (CFun freef_decl);
            add_fwd_decl free_f fwd_decl (CDefForwardFun (free_f, loc)))
        else ();
        if ktp_custom_copy then
            (set_idc_entry copy_f (CFun copyf_decl);
            add_fwd_decl copy_f fwd_decl (CDefForwardFun (copy_f, loc)))
        else ();
        {ctti_struct_decl=struct_decl;
        ctti_freef_decl=freef_decl;
        ctti_copyf_decl=copyf_decl;
        ctti_src_exp=src_exp;
        ctti_dst_exp=dst_exp;
        ctti_cname_wo_prefix=cname_wo_prefix}
    in
    let get_var_enum kvar =
        let {kvar_name; kvar_base_name; kvar_cases; kvar_flags; kvar_loc} = !kvar in
        match Env.find_opt kvar_base_name !all_var_enums with
        | Some(e_id) ->
            (match (cinfo_ e_id kvar_loc) with
            | CEnum {contents={ce_members}} -> (e_id, ce_members)
            | _ -> raise_compile_err kvar_loc (sprintf "invalid variant enumeration '%s'" (id2str e_id)))
        | _ ->
            let e_base_cname = (pp_id2str kvar_base_name) ^ "_" in
            let e_cname = e_base_cname ^ "tag_t" in
            let e_id = gen_temp_idc e_cname in
            let start_idx = if (List.mem VariantHaveNull kvar_flags) then 0 else 1 in
            let ctx = (CTypCInt, kvar_loc) in
            let (_, members) = List.fold_left (fun (idx, members) (ni, ti) ->
                let cname_i = pp_id2str ni in
                let cname_i = "_FX_" ^ e_base_cname ^ cname_i in
                let ni = dup_idc ni in
                let dv = { cv_name=ni; cv_typ=CTypCInt; cv_cname=cname_i; cv_flags=[]; cv_scope=ScGlobal::[]; cv_loc=kvar_loc } in
                let _ = set_idc_entry ni (CVal dv) in
                let vali = Some (CExpLit((LitInt (Int64.of_int idx)), ctx)) in
                (idx+1, (ni, vali) :: members)) (start_idx, []) kvar_cases in
            let members = List.rev members in
            let ce = ref { ce_name=e_id; ce_members=members;
                ce_cname=K_mangle.add_fx e_cname; ce_scope=ScGlobal::[]; ce_loc=kvar_loc } in
            set_idc_entry e_id (CEnum ce);
            all_var_enums := Env.add kvar_base_name e_id !all_var_enums;
            add_decl e_id (CDefEnum ce);
            (e_id, members)
    in
    (* converts named type to C *)
    let rec cvt2ctyp tn loc =
        (*let _ = printf "cvt2ctyp %s\n" (get_idk_cname tn loc) in*)
        if (IdSet.mem tn !all_decls) then ()
        else
            (*let _ = printf "proceeding with %s\n" (get_idk_cname tn loc) in*)
            let visited = IdSet.mem tn !all_visited in
            let deps = K_annotate_types.get_typ_deps tn loc in
            let kt_info = kinfo_ tn loc in
            let (ce_id, ce_members, recursive_variant, fwd_decl, deps) = match kt_info with
                | KVariant kvar ->
                    let (ce_id, ce_members) = get_var_enum kvar in
                    let {kvar_flags} = !kvar in
                    if (List.mem VariantRecursive kvar_flags) then
                        let fwd_decl = not (IdSet.mem tn !all_fwd_decls) in
                        (ce_id, ce_members, true, fwd_decl, (IdSet.filter (fun d -> d != tn) deps))
                    else
                        (ce_id, ce_members, false, false, deps)
                | _ -> (noid, [], false, false, deps) in
            let _ = match (recursive_variant, visited) with
                | (false, true) -> raise_compile_err loc
                    (sprintf "non-recursive variant type '%s' references itself directly or indirectly" (id2str tn))
                | _ -> () in
            let _ = all_visited := IdSet.add tn !all_visited in
            let ktp = K_annotate_types.get_ktprops (KTypName tn) loc in
            let fx_result_id = get_id "fx_result" in
            let fx_result_exp = make_id_exp fx_result_id loc in
            let fx_result_ct = CTypName tn in
            let {ctti_struct_decl=struct_decl; ctti_freef_decl=freef_decl; ctti_copyf_decl=copyf_decl;
                ctti_src_exp=src_exp; ctti_dst_exp=dst_exp; ctti_cname_wo_prefix=tp_cname_wo_prefix} =
                if not recursive_variant then create_ctyp_decl tn fwd_decl loc
                else (match (Env.find_opt tn !all_saved_rec_vars) with
                | Some i -> i
                | _ -> raise_compile_err loc
                    (sprintf "cgen: missing information about recursive variant '%s'"
                        (get_idk_cname tn loc)))
                in
            let {cf_name=free_f} = !freef_decl in
            let {cf_name=copy_f} = !copyf_decl in
            let {ct_props} = !struct_decl in
            let tp_cname = K_mangle.add_fx tp_cname_wo_prefix in
            let (struct_typ, struct_id_opt) = match !struct_decl with
                | {ct_typ=CTypRawPtr(_, (CTypStruct(struct_id_opt, _) as a_struct_typ))} ->
                    let struct_typ = match struct_id_opt with
                        | Some tn -> CTypName tn
                        | _ -> a_struct_typ
                        in
                    (struct_typ, struct_id_opt)
                | _ -> ((CTypName tn), (Some tn))
            in
            let process_deps () = IdSet.iter (fun dep ->
                if (Env.mem dep !all_saved_rec_vars) then ()
                else
                    let dep_loc = get_idk_loc dep loc
                    in cvt2ctyp dep dep_loc) deps
            in
            let decref_and_free dst_exp free_code loc =
                let rc_exp = CExpArrow(dst_exp, (get_id "rc"), (CTypInt, loc)) in
                let decref_exp = make_call !std_FX_DECREF [rc_exp] CTypInt loc in
                let cmp_1_exp = CExpBinOp(COpCompareEQ, decref_exp, (make_int_exp 1 loc), (CTypBool, loc)) in
                let cc_exp = CExpBinOp(COpLogicAnd, dst_exp, cmp_1_exp, (CTypBool, loc)) in
                let call_free = make_call !std_fx_free [dst_exp] CTypVoid loc in
                let then_exp = rccode2stmt ((CExp call_free) :: free_code) loc in
                let clear_ptr = CExpBinOp(COpAssign, dst_exp, (make_nullptr loc), (CTypVoid, loc)) in
                [(CExp clear_ptr); CStmtIf(cc_exp, then_exp, (CStmtNop loc), loc)]
            in

            all_decls := IdSet.add tn !all_decls;
            (*printf "deps for %s: " (id2str tn); IdSet.iter (fun d -> printf "%s, " (id2str d)) deps; printf "\n";*)
            process_deps();
            add_decl tn (CDefTyp struct_decl);
            if ktp.ktp_custom_free then
                add_decl free_f (CDefFun freef_decl)
            else ();
            if ktp.ktp_custom_copy then
                add_decl copy_f (CDefFun copyf_decl)
            else ();
            (*if not recursive_variant then process_deps() else ();*)
            match kt_info with
            | KTyp kt ->
                let {kt_typ; kt_loc} = !kt in
                (match kt_typ with
                | KTypTuple(telems) ->
                    let (_, free_code, copy_code, make_code, relems, make_argtyps, make_args) =
                        List.fold_left (fun (i, free_code, copy_code, make_code,
                            relems, make_argtyps, make_args) kti ->
                            let ni = get_id (sprintf "t%i" i) in
                            let cti = ktyp2ctyp kti kt_loc in
                            let selem = CExpArrow(src_exp, ni, (cti, loc)) in
                            let delem = CExpArrow(dst_exp, ni, (cti, loc)) in
                            let free_code = gen_free_code delem cti false false free_code kt_loc in
                            let copy_code = gen_copy_code selem delem cti copy_code kt_loc in
                            let ktpi = K_annotate_types.get_ktprops kti kt_loc in
                            let selem2 = (make_id_exp ni loc) in
                            let (cti_arg, selem2) = if ktpi.ktp_pass_by_ref then
                                    ((make_const_ptr cti), (cexp_deref selem2))
                                else (cti, selem2) in
                            let delem2 = CExpArrow(fx_result_exp, ni, (cti, loc)) in
                            let make_code = gen_copy_code selem2 delem2 cti make_code kt_loc in
                            (i+1, free_code, copy_code, make_code, (ni, cti) :: relems, cti_arg::make_argtyps, ni::make_args))
                            (0, [], [], [], [], [], []) telems in
                    let mktupl = if ktp.ktp_complex then
                        (let make_args = List.rev (fx_result_id :: make_args) in
                        let make_argtyps = List.rev ((make_ptr fx_result_ct) :: make_argtyps) in
                        let mktup_id = gen_temp_idc "mktup" in
                        let mktup_decl = ref {
                            cf_name=mktup_id; cf_typ=CTypFun(make_argtyps, CTypVoid);
                            cf_cname="_fx_make_" ^ tp_cname_wo_prefix;
                            cf_args=make_args; cf_body=(List.rev make_code);
                            cf_flags=FunNoThrow::[]; cf_scope=ScGlobal :: []; cf_loc=loc } in
                        set_idc_entry mktup_id (CFun mktup_decl);
                        add_decl mktup_id (CDefFun mktup_decl);
                        freef_decl := {!freef_decl with cf_body=List.rev free_code};
                        copyf_decl := {!copyf_decl with cf_body=List.rev copy_code};
                        mktup_id :: [])
                    else [] in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct((Some tn), List.rev relems);
                        ct_props={ct_props with ctp_make=mktupl}}
                | KTypRecord(_, relems) ->
                    let (free_code, copy_code, make_code, relems, make_argtyps, make_args) =
                        List.fold_left (fun (free_code, copy_code, make_code, relems, make_argtyps, make_args) (ni, kti) ->
                            let cti = ktyp2ctyp kti loc in
                            let selem = CExpArrow(src_exp, ni, (cti, kt_loc)) in
                            let delem = CExpArrow(dst_exp, ni, (cti, kt_loc)) in
                            let free_code = gen_free_code delem cti false false free_code kt_loc in
                            let copy_code = gen_copy_code selem delem cti copy_code kt_loc in
                            let ktpi = K_annotate_types.get_ktprops kti kt_loc in
                            let arg_ni = get_id ("r_" ^ (pp_id2str ni)) in
                            let selem2 = (make_id_exp arg_ni loc) in
                            let (cti_arg, selem2) = if ktpi.ktp_pass_by_ref then
                                    ((make_const_ptr cti), (cexp_deref selem2))
                                else (cti, selem2) in
                            let delem2 = CExpArrow(fx_result_exp, ni, (cti, loc)) in
                            let make_code = gen_copy_code selem2 delem2 cti make_code kt_loc in
                            (free_code, copy_code, make_code, (ni, cti) :: relems, cti_arg::make_argtyps, arg_ni::make_args))
                            ([], [], [], [], [], []) relems in
                    let mkrecl = if ktp.ktp_complex then
                        (let make_args = List.rev (fx_result_id :: make_args) in
                        let make_argtyps = List.rev ((make_ptr fx_result_ct) :: make_argtyps) in
                        let mkrec_id = gen_temp_idc "mktup" in
                        let mkrec_decl = ref {
                            cf_name=mkrec_id; cf_typ=CTypFun(make_argtyps, CTypVoid);
                            cf_cname="_fx_make_" ^ tp_cname_wo_prefix;
                            cf_args=make_args; cf_body=(List.rev make_code);
                            cf_flags=FunNoThrow::[]; cf_scope=ScGlobal :: []; cf_loc=loc } in
                        set_idc_entry mkrec_id (CFun mkrec_decl);
                        add_decl mkrec_id (CDefFun mkrec_decl);
                        freef_decl := {!freef_decl with cf_body=List.rev free_code};
                        copyf_decl := {!copyf_decl with cf_body=List.rev copy_code};
                        mkrec_id :: [])
                    else [] in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct((Some tn), List.rev relems);
                        ct_props={ct_props with ctp_make=mkrecl}}
                | KTypList et ->
                    let ct_hd = ktyp2ctyp et kt_loc in
                    let ct_tl = fx_result_ct in
                    let {ctp_free=(_, free_f)} = ct_props in
                    let relems = ((get_id "rc"), CTypInt) ::
                        ((get_id "tl"), ct_tl) ::
                        ((get_id "hd"), ct_hd) :: [] in
                    let (free_m, free_f) = if free_f = noid then (!std_FX_FREE_LIST_SIMPLE, !std_fx_free_list_simple) else
                        let f = make_id_exp !std_FX_FREE_LIST_IMPL loc in
                        let tn_arg = make_id_exp tn loc in
                        let free_hd_f = match (get_free_f ct_hd true false loc) with
                            | (_, Some(free_hd_f)) -> free_hd_f
                            | _ -> raise_compile_err loc
                                (sprintf "unexpected element destructor when converting %s" (get_idk_cname tn loc))
                        in let free_code = [CExp(CExpCall(f, tn_arg :: free_hd_f :: [], (CTypVoid, loc)))] in
                        (freef_decl := {!freef_decl with cf_body=free_code}; (noid, free_f)) in
                    let cons_id = gen_temp_idc "cons" in
                    let make_list_m = make_id_exp !std_FX_MAKE_LIST_IMPL kt_loc in
                    let (pass_by_ref, copy_hd_f) = match (get_copy_f ct_hd false true loc) with
                        | (pbr, Some(f)) -> (pbr, f)
                        | _ -> raise_compile_err loc
                            (sprintf "missing element copy operator when converting %s" (get_idk_cname tn loc)) in
                    let ct_hd_arg = if pass_by_ref then (make_const_ptr ct_hd) else ct_hd in
                    let make_list_body = CExp(CExpCall(make_list_m,
                        (make_id_exp (get_id tp_cname) loc) :: copy_hd_f :: [], (CTypInt, loc))) :: [] in
                    let cons_decl = ref {
                        cf_name=cons_id; cf_typ=CTypFun(ct_hd_arg :: ct_tl :: CTypBool :: (make_ptr ct_tl) :: [], CTypCInt);
                        cf_cname="_fx_cons_" ^ tp_cname_wo_prefix;
                        cf_args=(get_id "hd") :: (get_id "tl") :: (get_id "addref_tl") :: fx_result_id :: [];
                        cf_body=make_list_body;
                        cf_flags=[]; cf_scope=ScGlobal :: []; cf_loc=loc } in
                    set_idc_entry cons_id (CFun cons_decl);
                    add_decl cons_id (CDefFun cons_decl);
                    struct_decl := {!struct_decl with
                        ct_typ=(make_ptr (CTypStruct(struct_id_opt, relems)));
                        ct_props={ct_props with ctp_free=(free_m, free_f); ctp_make=cons_id::[]}}
                | KTypRef(et) ->
                    let ctyp = ktyp2ctyp et kt_loc in
                    let {ctp_free=(_, free_f)} = ct_props in
                    let relems = ((get_id "rc"), CTypInt) ::
                        ((get_id "data"), ctyp) :: [] in
                    let (free_m, free_f) = if free_f = noid then
                            (!std_FX_FREE_REF_SIMPLE, !std_fx_free_ref_simple)
                        else
                            let f = make_id_exp !std_FX_FREE_REF_IMPL loc in
                            let tn_arg = make_id_exp tn loc in
                            let free_hd_f = match (get_free_f ctyp true false loc) with
                                | (_, Some(free_hd_f)) -> free_hd_f
                                | _ -> raise_compile_err loc
                                    (sprintf "unexpected element destructor when converting %s" (get_idk_cname tn loc))
                            in let free_code = [CExp(CExpCall(f, tn_arg :: free_hd_f :: [], (CTypVoid, loc)))] in
                            (freef_decl := {!freef_decl with cf_body=free_code}; (noid, free_f)) in
                    let mkref_id = gen_temp_idc "mkref" in
                    let mkref_m = make_id_exp !std_FX_MAKE_REF_IMPL kt_loc in
                    let (pass_by_ref, copy_data_f) = match (get_copy_f ctyp false true loc) with
                        | (pbr, Some(f)) -> (pbr, f)
                        | _ -> raise_compile_err loc
                            (sprintf "missing element copy operator when converting %s" (get_idk_cname tn loc)) in
                    let ct_arg = if pass_by_ref then (make_const_ptr ctyp) else ctyp in
                    let mkref_body = CExp(CExpCall(mkref_m,
                        (make_id_exp (get_id tp_cname) loc) :: copy_data_f :: [], (CTypInt, loc))) :: [] in
                    let mkref_decl = ref {
                        cf_name=mkref_id; cf_typ=CTypFun(ct_arg :: (make_ptr fx_result_ct) :: [], CTypCInt);
                        cf_cname="_fx_make_" ^ tp_cname_wo_prefix;
                        cf_args=(get_id "arg") :: fx_result_id :: [];
                        cf_body=mkref_body;
                        cf_flags=[]; cf_scope=ScGlobal :: []; cf_loc=loc } in
                    set_idc_entry mkref_id (CFun mkref_decl);
                    add_decl mkref_id (CDefFun mkref_decl);
                    struct_decl := {!struct_decl with ct_typ=(make_ptr (CTypStruct(struct_id_opt, relems)));
                        ct_props={ct_props with ctp_free=(free_m, free_f); ctp_make=mkref_id::[]}}
                | KTypFun(argtyps, rt) ->
                    let cargs = ktyp2ctyp_fargs argtyps rt loc in
                    let fp_ctyp = CTypFunRawPtr(cargs, CTypCInt) in
                    let fv_ctyp = make_ptr (CTypName(get_id "fx_fcv_t")) in
                    let relems = ((get_id "fp"), fp_ctyp) :: ((get_id "fcv"), fv_ctyp) :: [] in
                    struct_decl := {!struct_decl with ct_typ=CTypStruct(struct_id_opt, relems);
                        ct_props={ct_props with ctp_ptr=false; ctp_pass_by_ref=true;
                                ctp_free=(!std_FX_FREE_FP, !std_fx_free_fp);
                                ctp_copy=(!std_FX_COPY_FP, !std_fx_copy_fp)}}
                | _ -> ())
            | KVariant kvar ->
                let {kvar_name; kvar_base_name; kvar_cases; kvar_flags; kvar_loc} = !kvar in
                let int_ctx = (CTypCInt, kvar_loc) in
                let void_ctx = (CTypVoid, kvar_loc) in
                let tag_id = get_id "tag" in
                let u_id = get_id "u" in
                let dst_exp = if recursive_variant then CExpUnOp(COpDeref, dst_exp, (struct_typ, kvar_loc)) else dst_exp in
                let src_tag_exp = CExpArrow(src_exp, tag_id, int_ctx) in
                let dst_tag_exp = CExpArrow(dst_exp, tag_id, int_ctx) in
                let src_u_exp = CExpArrow(src_exp, u_id, (CTypAny, kvar_loc)) in
                let dst_u_exp = CExpArrow(dst_exp, u_id, (CTypAny, kvar_loc)) in
                let (free_cases, copy_cases, uelems) =
                    List.fold_left2 (fun (free_cases, copy_cases, uelems) (ni, kt) (label_i, _) ->
                        match kt with
                        | KTypVoid -> (free_cases, copy_cases, uelems)
                        | _ ->
                            let ti = ktyp2ctyp kt loc in
                            let ni_clean = get_orig_id ni in
                            let selem_i = CExpMem(src_u_exp, ni_clean, (ti, kvar_loc)) in
                            let delem_i = CExpMem(dst_u_exp, ni_clean, (ti, kvar_loc)) in
                            let label_i_exp = [CExpIdent(label_i, int_ctx)] in
                            let free_code_i = gen_free_code delem_i ti false false [] kvar_loc in
                            let free_cases = match free_code_i with
                                | [] -> free_cases
                                | _ -> (label_i_exp, free_code_i) :: free_cases in
                            let copy_code_i = gen_copy_code selem_i delem_i ti [] kvar_loc in
                            let copy_cases = match copy_code_i with
                                | CExp(CExpBinOp(COpAssign, _, _, _)) :: [] -> copy_cases
                                | _ -> (label_i_exp, copy_code_i) :: copy_cases in
                            (free_cases, copy_cases, (ni_clean, ti) :: uelems)) ([], [], []) kvar_cases ce_members in
                let free_code = match free_cases with
                    | [] -> []
                    | _ ->
                        (* add "default: ;" case *)
                        let free_cases = ([], []) :: free_cases in
                        [CStmtSwitch(dst_tag_exp, (List.rev free_cases), kvar_loc)]
                    in
                let free_code = if recursive_variant then
                        decref_and_free dst_exp free_code kvar_loc
                    else
                        let clear_tag = CExpBinOp(COpAssign, dst_tag_exp, (make_int_exp 1 kvar_loc), void_ctx) in
                        if free_code = [] then
                            (CExp clear_tag) :: free_code
                        else []
                    in
                let copy_code = gen_copy_code src_tag_exp dst_tag_exp CTypCInt [] kvar_loc in
                let default_copy_code = CExp(CExpBinOp(COpAssign, dst_u_exp, src_u_exp, void_ctx)) in
                let copy_code = match copy_cases with
                    | [] -> default_copy_code :: copy_code
                    | _ ->
                        let copy_cases = ([], [default_copy_code]) :: copy_cases in
                        CStmtSwitch(src_tag_exp, (List.rev copy_cases), kvar_loc) :: copy_code
                in
                let relems = if uelems = [] then [] else (u_id, CTypUnion(None, List.rev uelems)) :: [] in
                let relems = (tag_id, CTypCInt) :: relems in
                if recursive_variant then
                    let relems = ((get_id "rc"), CTypInt) :: relems in
                    struct_decl := {!struct_decl with ct_typ=(make_ptr (CTypStruct(struct_id_opt, relems)))};
                else
                    struct_decl := {!struct_decl with ct_typ=CTypStruct(struct_id_opt, relems)};
                freef_decl := {!freef_decl with cf_body=List.rev free_code};
                copyf_decl := {!copyf_decl with cf_body=List.rev copy_code}
            | _ -> raise_compile_err loc (sprintf "type '%s' cannot be converted to C" (id2str tn))
    in
    let rec fold_n_cvt_ktyp t loc callb = ()
    and fold_n_cvt_kexp e callb =
        match e with
        | KDefTyp {contents={kt_name; kt_cname; kt_loc}} ->
            cvt2ctyp kt_name kt_loc
        | KDefVariant {contents={kvar_name; kvar_loc}} -> cvt2ctyp kvar_name kvar_loc
        | KDefClosureVars _ -> ()
        | KDefExn {contents={ke_name; ke_loc}} -> () (* [TODO] convert exception to C *)
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
        | _ -> fold_kexp e callb
    in let fold_n_cvt_callb =
    {
        kcb_fold_ktyp = Some(fold_n_cvt_ktyp);
        kcb_fold_kexp = Some(fold_n_cvt_kexp);
        kcb_fold_atom = None;
        kcb_fold_result = 0
    }
    in
    let _ = List.iter (fun e ->
        match e with
        | KDefVariant {contents={kvar_name; kvar_flags; kvar_loc}} ->
            if not (List.mem VariantRecursive kvar_flags) then ()
            else
                let i = create_ctyp_decl kvar_name true kvar_loc in
                all_saved_rec_vars := Env.add kvar_name i !all_saved_rec_vars
        | _ -> ()) top_code in
    let _ = List.iter (fun e -> fold_n_cvt_kexp e fold_n_cvt_callb) top_code in
    let ccode = (List.rev !top_fwd_decl) @ (List.rev !top_typ_decl) @ (List.rev !top_typfun_decl) in
    (*C_pp.pprint_top ccode;*)
    ccode
