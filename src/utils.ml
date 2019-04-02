(*
   Some utility functions used by the compiler.
   [TODO] when the compiler is rewritten in Ficus,
   those functions should be put to Ficus Std Lib
*)
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
