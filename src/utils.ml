(*
   Some utility functions used by the compiler.
   [TODO] when the compiler is rewritten in Ficus,
   those functions should be put to Ficus Std Lib
*)
open Ast

let is_some x_opt = match x_opt with Some _ -> true | _ -> false
let is_none x_opt = match x_opt with Some _ -> false | _ -> true

let zip l1 l2 = List.map2 (fun i1 i2 -> (i1, i2)) l1 l2

let rec last_elem l = match l with
    | x :: [] -> x
    | x :: rest -> last_elem rest
    | [] -> failwith "empty list"

let starts_with s subs =
    let l0 = String.length s in
    let l1 = String.length subs in
    l0 >= l1 && (String.sub s 0 l1) = subs

let trim_left s n =
    let n0 = String.length s in
    if n >= n0 then "" else String.sub s n (n0-n)

let rec normalize_path dir fname =
    let sep = Filename.dir_sep in
    let seplen = String.length sep in
    if not (Filename.is_relative fname) then fname else
    if (starts_with fname ("." ^ sep)) then (normalize_path dir (trim_left fname (1+seplen))) else
    if (starts_with fname (".." ^ sep)) then
    (let parent_dir = Filename.dirname dir in
    let fname1 = trim_left fname (2+seplen) in
    normalize_path parent_dir fname1) else
    (Filename.concat dir fname)

let remove_extension fname =
    try Filename.chop_extension fname with Invalid_argument _ -> fname

let dot_regexp = Str.regexp "\\."
let rec locate_module_file mname inc_dirs =
    let mfname = (Str.global_replace dot_regexp Filename.dir_sep mname) ^ ".fx" in
    try
        let mfname_full = Filename.concat
            (List.find (fun d -> Sys.file_exists (Filename.concat d mfname)) inc_dirs)
            mfname in
        Some(normalize_path (Sys.getcwd()) mfname_full)
    with Not_found -> None

let parser_ctx_file = ref noid
let parser_ctx_deps = ref ([] : id_t list)
let parser_ctx_inc_dirs= ref ([] : string list)

let update_imported_modules mname_id (pos0, pos1) =
    let mname = pp_id2str mname_id in
    match locate_module_file mname !parser_ctx_inc_dirs with
    | Some(mfname) ->
        let dep_minfo = find_module mname_id mfname in
        let mname_unique_id = !dep_minfo.dm_name in
        (parser_ctx_deps := mname_unique_id :: !parser_ctx_deps;
        mname_unique_id)
    | _ -> raise (SyntaxError(("module " ^ mname ^ " is not found"), pos0, pos1))

let ipower a b =
    let rec ipower_ a b p =
        if b = 0L then p else
            let p = if (Int64.logand b 1L) != 0L then (Int64.mul p a) else p in
            ipower_ (Int64.mul a a) (Int64.div b 2L) p in
    ipower_ a b 1L
