(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Transforms tail-recursive functions into loops.

    Note that some functions may call itself several times,
    where some of the calls may be tail calls whereas the others may be
    non-tail calls. The classical example is qsort. In those cases
    the algorithm retains non-tail calls as recursive calls and
    transforms the function body in such a way that all tail calls
    update the parameters and 'jump' into the beginning of the function.
*)
open Ast
open K_form

let tailrec2loop kf =
    let { kf_name; kf_typ; kf_args; kf_body; kf_flags; kf_scope; kf_loc } = !kf in
    let rec have_tailrec_calls_ e =
        match e with
        | KExpSeq(elist, _) ->
            elist != [] && have_tailrec_calls_ (Utils.last_elem elist)
        | KExpIf(_, then_e, else_e, _) ->
            (have_tailrec_calls_ then_e) ||
            (have_tailrec_calls_ else_e)
        | KExpCall(f, _, _) -> f = kf_name
        | KExpMatch (cases, _) ->
            List.exists (fun (_, e) -> have_tailrec_calls_ e) cases
        | KExpTryCatch(try_e, catch_e, _) ->
            (have_tailrec_calls_ try_e) ||
            (have_tailrec_calls_ catch_e)
        | _ -> false
    in if not (have_tailrec_calls_ kf_body) then ()
    else
        (* the function has some tail recursive calls;
           let's transform the whole loop body into a loop
           where the tail calls update the parameter values and
           jump to the beginning of the loop *)
        let (argtyps, rt, res_n, f_init_code) =
            match kf_typ with
            | KTypFun(argtyps, KTypVoid) -> (argtyps, KTypVoid, noid, [])
            | KTypFun(argtyps, rt) ->
                let res_n = gen_temp_idk "res" in
                let res_val0 = KExpAtom((Atom.Lit LitNil), (rt, kf_loc)) in
                let f_init_code = create_defval res_n rt (ValMutable :: [])
                    (Some res_val0) [] kf_scope kf_loc in
                (argtyps, rt, res_n, f_init_code)
            | _ -> raise_compile_err kf_loc
                (sprintf "the function '%s' has invalid (non-function type)" (id2str kf_name))
            in
        let nargs = List.length kf_args in
        let nargtyps = List.length argtyps in
        let _ = if nargs = nargtyps then ()
            else raise_compile_err kf_loc
                (sprintf "function '%s': the number of arguments (=%d) and its types (=%d) do not match"
                (id2str kf_name) nargs nargtyps) in
        (* For each function argument we create 2 aliases.
            The first will become the new function formal argument.
            The second will become a variable to which we assign a new value in the case of 'tail call'.

            before:
            fun foo(a: at, b: bt, c: ct) {
                if (a < 0) {
                    ...
                    foo(a+1, b-1, c*2) // tail-recursive call
                }
                else {
                    ...
                    bar(b+c) // return the value
                }
            }

            after:
            // rename a->a', b->b', c->c'; a', b' and 'c are still immutable
            fun foo(a': at, b': bt, c': ct) {
                var res = nil // initialize the function result (non-void function case)
                // and these are mutable copies of the arguments
                var a'' = a', b'' = b', c'' = c'
                while(true) {
                    // before processing save them as immutable values,
                    // so that the body semantics is preserved
                    val a=a'', b=b'', c=c''
                    // the body is mostly retained as-is
                    if (a < 0) {
                        ...
                        // replace tail-recursive calls
                        a''=a+1; b''=b-1; c''=c*2;
                        // continue; // continue is not needed here,
                                     // because it's the tail call
                    }
                    else {
                        ...
                        res = bar(b+c); break
                        // or 'bar(b+c); break' in the case of void function
                    }
                }
                res // return the final result
            }
            Why have two aliases and not just turn a, b, c into variables?
            This is because we want to keep the original semantics of the body code
            (where each function argument is an immutable value) and also want to
            enable all the optimizations that compiler can do with immutable values.
        *)
        let loop_scope = new_block_scope() :: kf_scope in
        let (new_kf_args, trec_args, f_init_code, loop_init_code) =
            List.fold_left2 (fun (new_kf_args, trec_args, f_init_code, loop_init_code) ai ti ->
                let dv0 = match (kinfo_ ai kf_loc) with
                    | KVal dv -> dv
                    | _ -> raise_compile_err kf_loc
                        (sprintf "incorrect information about parameter '%s' of function '%s'. It should be KVal"
                            (id2str ai) (id2str kf_name)) in
                let a1i = dup_idk ai in
                let a2i = dup_idk ai in
                let dv1 = { dv0 with kv_name=a1i } in
                let _ = set_idk_entry a1i (KVal dv1) in
                let a1i_as_exp = KExpAtom((Atom.Id a1i), (ti, kf_loc)) in
                let a2i_as_exp = KExpAtom((Atom.Id a2i), (ti, kf_loc)) in
                let f_init_code = create_defval a2i ti (ValMutable :: [])
                    (Some a1i_as_exp) f_init_code kf_scope kf_loc in
                let loop_init_code = create_defval ai ti []
                    (Some a2i_as_exp) loop_init_code loop_scope kf_loc in
                (a1i :: new_kf_args, (a2i, ti) :: trec_args, f_init_code, loop_init_code))
            ([], [], f_init_code, []) kf_args argtyps in
        let new_kf_args = List.rev new_kf_args in
        let trec_args = List.rev trec_args in
        (* the function prologue is ready; the loop prologue is ready;
           now let's transform the function body *)
        let process_func_ending final_e =
            match final_e with
            | KExpThrow _ -> final_e
            | _ ->
                let eloc = get_kexp_loc final_e in
                let final_e = if res_n = noid
                    then final_e
                    else KExpAssign(res_n, final_e, eloc) in
                KExpSeq((final_e :: (KExpBreak eloc) :: []), (KTypVoid, eloc))
        in let rec transform_tcalls e =
            match e with
            | KExpSeq(elist, (_, eloc)) ->
                let rcode = match (List.rev elist) with
                    | [] -> (KExpBreak eloc) :: []
                    | final_e :: rest ->
                        (transform_tcalls final_e) :: rest
                in rcode2kexp rcode eloc
            | KExpIf(c, then_e, else_e, kctx) ->
                let then_e = transform_tcalls then_e in
                let else_e = transform_tcalls else_e in
                KExpIf(c, then_e, else_e, kctx)
            | KExpCall(f, real_args, (_, eloc)) ->
                if f = kf_name then
                    let tcall_rcode = List.fold_left2 (fun tcall_rcode (trec_ai, ti) real_ai ->
                        let set_new = KExpAssign(trec_ai, (KExpAtom(real_ai, (ti, eloc))), eloc) in
                        set_new :: tcall_rcode) [] trec_args real_args in
                    rcode2kexp tcall_rcode eloc
                else
                    process_func_ending e
            | KExpMatch(cases, kctx) ->
                let cases = List.map (fun (checks_i, e_i) ->
                    let e_i = transform_tcalls e_i in
                    (checks_i, e_i)) cases in
                KExpMatch(cases, kctx)
            | KExpTryCatch(try_e, catch_e, kctx) ->
                let try_e = transform_tcalls try_e in
                let catch_e = transform_tcalls catch_e in
                KExpTryCatch(try_e, catch_e, kctx)
            | _ ->
                process_func_ending e
        in
        let body_ = transform_tcalls kf_body in
        let loop_body = rcode2kexp (body_ :: loop_init_code) kf_loc in
        let lloc = get_kexp_loc loop_body in
        let loop_exp = KExpWhile((KExpAtom((Atom.Lit (LitBool true)),
            (KTypBool, lloc))), loop_body, lloc) in
        let f_code = (if res_n = noid then [] else
            KExpAtom((Atom.Id res_n), (rt, kf_loc)) :: []) @
            (loop_exp :: f_init_code) in
        let new_kf_body = rcode2kexp f_code kf_loc in
        kf := { !kf with kf_args=new_kf_args; kf_body=new_kf_body }

let tailrec2loops top_code =
    let rec tailrec2loop_ktyp_ t loc callb = t
    and tailrec2loop_kexp_ e callb =
        let e = walk_kexp e callb in
        (match e with
        | KDefFun kf ->
            tailrec2loop kf
        | _ -> ()); e in
    let trec2loop_callb =
    {
        kcb_atom=None;
        kcb_typ=Some(tailrec2loop_ktyp_);
        kcb_exp=Some(tailrec2loop_kexp_)
    } in
    List.map (fun e -> tailrec2loop_kexp_ e trec2loop_callb) top_code
