(*
   Some utility functions used by the compiler.
   [TODO] when the compiler is rewritten in Ficus,
   those functions should be put to Ficus Std Lib
*)
open Syntax

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

let find_module mname_id mfname =
    try get_module (Hashtbl.find all_modules mfname) with
    Not_found ->
        let m_fresh_id = get_fresh_id mname_id in
        let newmodule = ref { dm_name=m_fresh_id; dm_filename=mfname; dm_defs=[];
                              dm_deps=[]; dm_env=Env.empty; dm_parsed=false } in
        let _ = set_id_entry m_fresh_id (IdModule newmodule) in
        let _ = Hashtbl.add all_modules mfname m_fresh_id in
        newmodule

let update_imported_modules mname_id (pos0, pos1) =
    let mname = pp_id2str mname_id in
    match locate_module_file mname !current_inc_dirs with
    | Some(mfname) ->
        let dep_minfo = find_module mname_id mfname in
        let mname_unique_id = !dep_minfo.dm_name in
        (current_imported_modules := mname_unique_id :: !current_imported_modules;
        mname_unique_id)
    | _ -> raise (SyntaxError(("module " ^ mname ^ " is not found"), pos0, pos1))
