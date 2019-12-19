(*
    Convert all non-primitive types to KTypName(...)
    (including lists, arrays, references, tuples, records etc.).
    This is a useful step to make K-form closer to the final C code.
*)

open Ast
open K_form

type mangle_map_t = (string, id_t) Hashtbl.t

(* [TODO] when we add support for non-English characters in the identifiers,
    they should be "transliterated" into English *)
let mangle_name n loc =
    let info = kinfo_ n loc in
    let sc = get_kscope info in
    let rec full_name_ sc result =
        match sc with
        | ScModule(m) :: rest ->
            let mstr = pp_id2str m in
            let result = if mstr = "Builtins" then result else mstr :: result in
            full_name_ rest result
        | _ :: rest -> full_name rest result
        | [] -> result
        in
    String.concat "__" (full_name_ sc [pp_id2str n])

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
            make_unique (idx + 1)
        else
            Hashtbl.add mangle_map candidate n_id;
            candidate
        in
    make_unique_ 0

let rec mangle_ktyp t mangle_map loc =
    let rec mangle_typname_ n result =
        let info = kinfo_ n loc in
        let cname = get_kinfo_cname info loc in
        if cname != "" then cname else
        match info with
        | KVariant kvar ->
            let {kvar_name; kvar_targs; kvar_loc} = !kvar in
            let name = mangle_name kvar_name kvar_loc in
            let nargs = List.length kvar_targs in
            let result = List.fold_left
                (fun result targ -> mangle_ktyp_ targ result)
                [] kvar_targs in
            let suffix = (string_of_int nargs) ^ (String.concat "__" (List.rev result)) in
            let cname = mangle_make_unique kvar_name "V" name suffix mangle_map in
            kvar := { !kvar with kvar_cname=cname };
            cname
        | KRecord krec ->
            let {krec_name; krec_loc} = !krec in
            let name = mangle_name krec_name krec_loc in
            let cname = mangle_make_unique krec_name "R" name "" mangle_map in
            krec := { !krec with krec_cname=cname };
            cname
        | _ -> raise_compile_err loc "unsupported typename"
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
        | KTypFloat(32) -> "d" :: result
        | KTypFloat n -> raise_compile_err loc (sprintf "unsupported typ KTypFloat(%d)" n)
        | KTypVoid -> "v" :: result
        | KTypNil -> "z" :: result
        | KTypBool -> "B" :: result
        | KTypChar -> "C" :: result
        | KTypString -> "S" :: result
        | KTypCPointer -> "p" :: result
        | KTypFun(args, rt) ->
            let result = mangle_ktyp_ rt ("F" :: result) in
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
    in String.concat (List.rev (mangle_ktyp_ t []))

let mangle_all top_code =
    let mangle_map = (Hashtbl.create 1000 : mangle_map_t) in
    let create_gen_typ prefix t loc =

    let rec walk_n_mangle_ktyp t callb =
        match t with
        | KTypInt | KTypSInt _ | KTypUInt _ | KTypFloat _
        | KTypVoid | KTypNil | KTypBool | KTypChar
        | KTypString | KTypCPointer | KTypExn
        | KTypErr | KTypModule -> t
        | KTypName n -> ignore(mangle_ktyp t mangle_map noloc); t
        | KTypRecord(rn, _) -> ignore(mangle_ktyp t mangle_map noloc); KTypName rn
        | KTypFun _ | KTypTuple _ | KTypClosure _
        | KTypArray _ | KTypList _ | KTypRef _ ->



    let mangle_kexp