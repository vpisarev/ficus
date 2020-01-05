(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Convert all non-primitive types to KTypName(...)
    (including lists, arrays, references, tuples, records etc.).
    This is a useful step to make K-form closer to the final C code.
*)

open Ast
open K_form

type mangle_map_t = (string, id_t) Hashtbl.t

let rec mangle_scope sc result loc =
    match sc with
    | ScModule(m) :: rest ->
        let mstr = pp_id2str m in
        let result = if mstr = "Builtins" then result else
                     if result = "" then mstr else mstr ^ "__" ^ result in
        mangle_scope rest result loc
    | _ :: rest -> mangle_scope rest result loc
    | [] -> result

(* [TODO] when we add support for non-English characters in the identifiers,
    they should be "transliterated" into English *)
let mangle_name n sc_opt loc =
    let sc = match sc_opt with
        | Some sc -> sc
        | _ -> get_kscope (kinfo_ n loc) in
    let prefix = mangle_scope sc "" loc in
    let nstr = pp_id2str n in
    if prefix = "" then nstr else prefix ^ "__" ^ nstr

(* try to compress the name by encoding the module name just once;
   for now it's used only for functions *)
let compress_name nstr sc loc =
    let prefix = mangle_scope sc "" loc in
    if prefix = "" then nstr
    else
        let prefix_ = prefix ^ "__" in
        let prefix_len = String.length prefix_ in
        let rx = Str.regexp ("\\([FVR]t*\\)\\([0-9]+\\)" ^ prefix_) in
        let new_nstr = Str.global_substitute rx (fun s ->
            let c = Str.matched_group 1 s in
            let len1 = int_of_string (Str.matched_group 2 s) in
            c ^ "M" ^ (string_of_int (len1 - prefix_len))) nstr in
        if new_nstr = nstr then nstr else
        "M" ^ (string_of_int (String.length prefix)) ^ prefix ^ new_nstr

(* Try if <prefix><nlen><name><suffix> is unique.
   If yes, add it to the set of mangled names and output.
   Otherwise, try <prefix><nlen1><name>1_<suffix>,
   then <prefix><nlen2><name>2_<suffix> etc.
   e.g. with prefix="V", name="rbtree" and suffix="1i"
   first try V6rbtree1i, then V8rbtree1_1i, V8rbtree2_1i, ..., V9rbtree10_1i etc.
   Note, that the name is preceded with its length
   (that includes the possible "1_" etc. in the end) *)
let mangle_make_unique n_id prefix name suffix mangle_map =
    let rec make_unique_ idx =
        let idxstr = if idx = 0 then "" else (string_of_int idx) ^ "_" in
        let name1 = name ^ idxstr in
        let nlen = String.length name1 in
        let candidate = prefix ^ (string_of_int nlen) ^ name1 ^ suffix in
        if Hashtbl.mem mangle_map candidate then
            make_unique_ (idx + 1)
        else
            (Hashtbl.add mangle_map candidate n_id;
            candidate)
        in
    make_unique_ 0

(* Convert type to a string, i.e. mangle it.
   Use mangle_map to control uniqueness when mangling KTypName _ and KTypRecord _.
   Update the mangled names (cname), if needed,
   for those KTypName _ and KTypRecord _. *)
let rec mangle_ktyp t mangle_map loc =
    let rec mangle_inst_ n_id prefix targs name sc =
        let nargs = List.length targs in
        let result = List.fold_left
            (fun result targ -> mangle_ktyp_ targ result)
            [] targs in
        let (prefix, suffix) = if nargs = 0 then (prefix, "") else
            ((prefix ^ "t"), ((string_of_int nargs) ^ (String.concat "" (List.rev result)))) in
        let name = mangle_name name (Some sc) loc in
        mangle_make_unique n_id prefix name suffix mangle_map
    and mangle_typname_ n result =
        match (kinfo_ n loc) with
        | KVariant kvar ->
            let {kvar_name; kvar_cname; kvar_targs; kvar_scope} = !kvar in
            let cname = if kvar_cname = "" then
                mangle_inst_ kvar_name "V" kvar_targs kvar_name kvar_scope
                else kvar_cname in
            kvar := {!kvar with kvar_cname=cname};
            cname :: result
        | KRecord krec ->
            let {krec_name; krec_cname; krec_targs; krec_scope} = !krec in
            let cname = if krec_cname = "" then mangle_inst_ krec_name "R" krec_targs krec_name krec_scope
                else krec_cname in
            krec := {!krec with krec_cname=cname};
            cname :: result
        | KGenTyp {contents={kgen_cname}} ->
            if kgen_cname = "" then
                raise_compile_err loc "KGenTyp does not have a proper mangled name"
            else kgen_cname :: result
        | _ ->
            raise_compile_err loc (sprintf "unsupported type '%s' (should be variant or record)" (id2str n))
    and mangle_ktyp_ t result =
        match t with
        | KTypInt -> "i" :: result
        | KTypSInt(8) -> "c" :: result
        | KTypSInt(16) -> "s" :: result
        | KTypSInt(32) -> "n" :: result
        | KTypSInt(64) -> "l" :: result
        | KTypSInt n -> raise_compile_err loc (sprintf "unsupported typ KTypSInt(%d)" n)
        | KTypUInt(8) -> "b" :: result
        | KTypUInt(16) -> "w" :: result
        | KTypUInt(32) -> "u" :: result
        | KTypUInt(64) -> "q" :: result
        | KTypUInt n -> raise_compile_err loc (sprintf "unsupported typ KTypUInt(%d)" n)
        | KTypFloat(16) -> "h" :: result
        | KTypFloat(32) -> "f" :: result
        | KTypFloat(64) -> "d" :: result
        | KTypFloat n -> raise_compile_err loc (sprintf "unsupported typ KTypFloat(%d)" n)
        | KTypVoid -> "v" :: result
        | KTypNil -> "z" :: result
        | KTypBool -> "B" :: result
        | KTypChar -> "C" :: result
        | KTypString -> "S" :: result
        | KTypCPointer -> "p" :: result
        | KTypFun(args, rt) ->
            let result = mangle_ktyp_ rt ("FP" :: result) in
            let result = (string_of_int (List.length args)) :: result in
            List.fold_left (fun result a -> mangle_ktyp_ a result) result args
        | KTypTuple(elems) ->
            let nelems = List.length elems in
            let nstr = string_of_int nelems in
            (match elems with
            | t0 :: rest ->
                if List.for_all (fun t -> t = t0) rest then
                    mangle_ktyp_ t0 (nstr :: "Ta" :: result)
                else
                    List.fold_left (fun result t -> mangle_ktyp_ t result) (nstr :: "T" :: result) elems
            | _ -> raise_compile_err loc "the tuple has 0 elements")
        (* treat the closure type just like normal function type, because after the lambda
            lifting all the 'function pointers' are 'closures' *)
        | KTypClosure(ftyp, _) -> mangle_ktyp_ ftyp result
        | KTypRecord(rn, _) -> mangle_typname_ rn result
        | KTypName(n) -> mangle_typname_ n result
        | KTypArray(dims, t) ->
            let result =  (string_of_int dims) :: "A" :: result in
            mangle_ktyp_ t result
        | KTypList(t) -> mangle_ktyp_ t ("L" :: result)
        | KTypRef(t) -> mangle_ktyp_ t ("r" :: result)
        | KTypExn -> "e" :: result
        | KTypErr -> raise_compile_err loc "KTypErr cannot be mangled"
        | KTypModule -> raise_compile_err loc "KTypModule cannot be mangled"
    in String.concat "" (List.rev (mangle_ktyp_ t []))

let mangle_all top_code =
    let mangle_map = (Hashtbl.create 1000 : mangle_map_t) in
    let curr_top_code = ref ([]: kexp_t list) in
    let create_gen_typ t name_prefix loc =
        let cname = mangle_ktyp t mangle_map loc in
        try
            let i = Hashtbl.find mangle_map cname in
            KTypName i
        with Not_found ->
            let i = gen_temp_idk name_prefix in
            let kg = ref { kgen_name=i; kgen_cname=cname; kgen_typ=t;
                kgen_scope=ScGlobal::[]; kgen_loc=loc } in
            Hashtbl.add mangle_map cname i;
            set_idk_entry i (KGenTyp kg);
            curr_top_code := (KDefGenTyp kg) :: !curr_top_code;
            KTypName i
        in
    let rec walk_ktyp_n_mangle t loc callb =
        let t = walk_ktyp t loc callb in
        match t with
        | KTypInt | KTypSInt _ | KTypUInt _ | KTypFloat _
        | KTypVoid | KTypNil | KTypBool | KTypChar
        | KTypString | KTypCPointer | KTypExn
        | KTypErr | KTypModule -> t
        | KTypName n -> ignore(mangle_ktyp t mangle_map loc); t
        | KTypRecord(rn, _) -> ignore(mangle_ktyp t mangle_map loc); KTypName rn
        | KTypFun _ -> create_gen_typ t "fun" loc
        | KTypClosure (ftyp, i) -> KTypClosure((walk_ktyp_n_mangle ftyp loc callb), i)
        | KTypTuple _ -> create_gen_typ t "tup" loc
        | KTypArray _ -> create_gen_typ t "arr" loc
        | KTypList _ -> create_gen_typ t "lst" loc
        | KTypRef _ -> create_gen_typ t "ref" loc
    and mangle_ktyp_retain_record t loc callb =
        match t with
        | KTypRecord(rn, relems) ->
            KTypRecord(rn, List.map (fun (ni, ti) -> (ni, walk_ktyp_n_mangle ti loc callb)) relems)
        | t -> walk_ktyp_n_mangle t loc callb
    and walk_kexp_n_mangle e callb =
        match e with
        | KDefVal(n, e, loc) ->
            let e = walk_kexp_n_mangle e callb in
            (match (kinfo_ n loc) with
            | KVal kv ->
                let {kv_typ} = kv in
                let t = walk_ktyp_n_mangle kv_typ loc callb in
                set_idk_entry n (KVal {kv with kv_typ=t})
            | _ -> raise_compile_err loc (sprintf "invalid description of '%s'; should be KVal _" (id2str n)));
            KDefVal(n, e, loc)
        | KDefFun kf ->
            let {kf_name; kf_typ; kf_args; kf_closure; kf_body; kf_scope; kf_loc} = !kf in
            let t = walk_ktyp kf_typ kf_loc callb in
            let t = match t with
                | KTypFun (_, _) -> t
                | _ -> KTypFun([], t)
                in
            let suffix = mangle_ktyp t mangle_map kf_loc in
            let suffix = String.sub suffix 2 ((String.length suffix) - 2) in
            let new_body = walk_kexp_n_mangle kf_body callb in
            let bare_name = mangle_name kf_name (Some kf_scope) kf_loc in
            let cname = mangle_make_unique kf_name "F" bare_name suffix mangle_map in
            let cname = compress_name cname kf_scope kf_loc in
            kf := { !kf with kf_cname=cname; kf_typ=t; kf_body=new_body };
            e
        | KDefExn ke ->
            let {ke_name; ke_typ; ke_scope; ke_loc} = !ke in
            let t = mangle_ktyp_retain_record ke_typ ke_loc callb in
            let suffix = mangle_ktyp t mangle_map ke_loc in
            let bare_name = mangle_name ke_name (Some ke_scope) ke_loc in
            let cname = mangle_make_unique ke_name "E" bare_name suffix mangle_map in
            ke := { !ke with ke_cname=cname; ke_typ=t };
            e
        | KDefVariant kvar ->
            let {kvar_name; kvar_cases; kvar_loc} = !kvar in
            (* compute and set kvar_cname *)
            let _ = mangle_ktyp (KTypName kvar_name) mangle_map kvar_loc in
            let var_cases = List.map (fun (n, t) ->
                (n, mangle_ktyp_retain_record t kvar_loc callb)) kvar_cases in
            kvar := { !kvar with kvar_cases=var_cases };
            e
        | KDefRecord krec ->
            let {krec_name; krec_elems; krec_loc} = !krec in
            (* compute and set krec_cname *)
            let _ = mangle_ktyp (KTypName krec_name) mangle_map krec_loc in
            let relems = match (mangle_ktyp_retain_record (KTypRecord(krec_name, krec_elems)) krec_loc callb) with
                | KTypRecord(_, relems) -> relems
                | _ -> raise_compile_err krec_loc "after mangling record is not a record anymore"
                in
            krec := { !krec with krec_elems=relems };
            e
        | KDefGenTyp _ ->
            (* since KDefGenTyp's are formed during this step, we should not get here.
               If we are here, retain the definition as-is *)
            e
        | _ -> walk_kexp e callb
        in
    let walk_n_mangle_callb =
    {
        kcb_typ=Some(walk_ktyp_n_mangle);
        kcb_exp=Some(walk_kexp_n_mangle);
        kcb_atom=None
    } in

    List.iter (fun e ->
        let e = walk_kexp_n_mangle e walk_n_mangle_callb in
        curr_top_code := e :: !curr_top_code) top_code;
    List.rev !curr_top_code
