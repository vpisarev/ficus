(*
    Full-scale lambda lifting.
    Unlike k_simple_ll, this algorithm
    moves all the functions to the top level.

    Also, unlike k_simple_ll, this algorithm is executed after all
    other K-form optimizations, including dead code elimination,
    tail-recursive call elimination, inline expansion etc.

    In order to move all the functions to the top level and preserve
    semantics of the original code, we need to transform some nested functions,
    as well as some outer code that uses those functions.

    * We analyze each function and see if the function has 'free variables',
      i.e. variables that are non-local and yet are non-global.
    a) If the function has some 'free variables',
      it needs a special satellite structure called 'closure data'
      that incapsulates all the free variables. The function itself
      is transformed, it gets an extra parameter, which is the
      closure data. All the accesses to free variables are replaced
      with the closure data access operations. Then, when the function
      occurs in code (if it does not occur, it's eliminated as dead code),
      a 'closure' is created, which is a pair (function, closure_data).
      This pair is used instead of the original function. Here is the example:

      fun foo(n: int) {
        fun bar(m: int) = m * n
        bar
      }

      is replaced with

      fun bar(m: int, c: bar_closure_t) {
        m * c->n
      }
      fun foo(n: int) {
        make_closure(bar, bar_closure_t {n})
      }

    b) If the function does not have any 'free variables', we may still
      need a closure, i.e. we may need to represent this function as a pair,
      because in general when we pass a function as parameter to another function
      or store it as a value (essentially, we store a function pointer), the caller
      of that function does not know whether it needs free variables or not, so
      we need a consistent representation of functions that are called indirectly.
      But in this case we can have a pair ('some function', nil), i.e. we just use something like
      NULL pointer instead of a pointer to some real closure. So, the following code:

      fun foo(n: int) {
        fun bar(m: int) = m*n
        fun baz(m: int) = m+1
        if (generate_random_number() % 2 == 0) bar else baz
      }
      val transform_f = foo(5)
      for (i <- 0:10) println(transform_f(i))

      is transformed to:

      fun bar( m: int, c: bar_closure_t* ) = m*c->n
      fun baz( m: int, _: nil_closure_t* ) = m+1

      fun foo(n: int) =
        if (generate_random_number() % 2 == 0)
          make_closure(bar, bar_closure_t {n})
        else
          make_closure(baz, nil)

      val (transform_f_ptr, transform_f_fvars) = foo(5)
      for (i <- 0:10) println(transform_f_ptr(i, transform_f_fvars))

      However, in the case (b) when we call the function directly, e.g.
      we call 'baz' as 'baz', not via 'transform_f' pointer, we can
      avoid the closure creation step and just call it as 'baz(real_arg, nil)'.

    From the above description it may seem that the algorithm is very simple,
    but there are some nuances:

    1. the nested function with free variables may not just read some values
      declared outside, it may modify mutable values, i.e. var's.
      Or, it may read from a 'var', and yet it may call another nested function
      that may access the same 'var' and modify it. We could have stored an address
      of each var in the closure data, but that would be unsafe, because we may
      return the created closure outside of the function where 'var' does not
      exist anymore (a typical functional language pattern for generators).
      The robust solution for this problem is to convert each 'var',
      which is used at least once as a free variable, into a reference:

      fun create_inc(start: int) {
          var v = start
          fun inc_me() {
            val temp = v; v += 1; temp
          }
          inc_me
      }

      this is converted to:

      fun inc_me( c: inc_me_closure_t* ) {
          val temp = *c->v; *c->v += 1; temp
      }
      fun create_inc(start: int) {
          val v = ref(start)
          make_closure(inc_me, inc_me_closure_t {v})
      }

    2. besides the free variables, the nested function may also call:
       2a. itself. This is a simple case. We just call it and pass the same closure data
          fun bar( n: int, c: bar_closure_t* ) = if (n <= 1) 1 else { ... bar(n-1, c) }
       2b. another function that needs some free variables from the outer scope
          fun foo(n: int) {
              fun bar(m: int) = baz(m+1)
              fun baz(m: int) = m*n
              (bar, baz)
          }

          in order to form the closure for 'baz', 'bar' needs to read 'n' value, which it does not
          access directly. That is, the code can be converted to:

          // option 1: dynamically created closure
          fun bar( m: int, c:bar_closure_t* ) {
              val (baz_cl_f, baz_cl_fv) = make_closure(baz, baz_closure_t {c->n})
              baz_cl_f(m+1, baz_cl_fv)
          }
          fun baz( m: int, c:baz_closure_t* ) = m*c->n
          fun foo(n: int) = (make_closure(bar, bar_closure_t {n}), make_closure(baz, baz_closure_t {n})

          or it can be converted to

          // option 2: nested closure
          fun bar( m: int, c:bar_closure_t* ) {
              val (baz_cl_f, baz_cl_fv) = c->baz_cl
              baz_cl_f(m+1, baz_cl_fv)
          }
          fun baz( m: int, c:baz_closure_t* ) = m*c->n
          fun foo(n: int) = {
              val baz_cl = make_closure(baz, baz_closure_t {n})
              val bar_cl = make_closure(bar, {baz_cl})
              (bar_cl, baz_cl)
          }

          or it can be converted to

          // option 3: shared closure
          fun bar( m: int, c:foo_nested_closure_t* ) {
              baz(m+1, c)
          }
          fun baz( m: int, c:foo_nested_closure_t* ) = m*c->n
          fun foo(n: int) = {
              val foo_nested_closure_data = foo_nested_closure_t {n}
              val bar_cl = make_closure(bar, foo_nested_closure_data)
              val baz_cl = make_closure(baz, foo_nested_closure_data)
              (bar_cl, baz_cl)
          }

        The third option in this example is the most efficient. But in general it may
        be not very easy to implement, because between bar() and baz() declarations there
        can be some value definitions, i.e. in general baz() may access some values that
        are computed using bar(), and then the properly initialized shared closure may be
        difficult to build.

        The second option is also efficient, because we avoid repetitive call to
        make_closure() inside bar(). However if not only 'bar' calls 'baz',
        but also 'baz' calls 'bar', it means that both closures need to reference each other,
        so we have a reference cycle and this couple of closures
        (or a cluster of closures in more general case) will never be released.

        So, for simplicity, we just implement the first, i.e. the slowest option.
        It's not a big problem though, because:
        * the language makes an active use of dynamic data structures
          (recursive variants, lists, arrays, strings, references ...) anyway,
          and so the memory allocation is used often, but it's tuned to be efficient,
          especially for small memory blocks.
        * when we get to the lambda lifting
          stage, we already have expanded many function calls inline
        * the remaining non-expanded nested functions that do not need
          free variables are called directly without creating a closure
        * when we have some critical hotspots, we can transform the critical functions and
          pass some 'free variables' as parameters in order to eliminate closure creation.
          (normally hotspot functions operate on huge arrays and/or they can be
          transformed into tail-recursive functions, so 'mutually-recursive functions'
          and 'hotspots' are the words that rarely occur in the same sentence).
        [TODO] The option are not mutually exclusive; for example, we can use the third,
        most efficient option in some easy-to-detect partial cases and use
        the first option everywhere else.

    3. In order to implement the first option (2.2b.1) above we need to create an
      iterative algorithm to compute extended sets of free variables for each function.
      First, we find directly accessed free variables. Then we check which functions
      we call from each function and combine their free variables with the directly
      accessible ones. We continue to do so until all the sets of free variables
      for all the functions are stabilized and do not change on the next iteration.
*)

open Ast
open K_form

type ll_func_info_t = { mutable ll_fvars: IdSet.t; ll_declared_inside: IdSet.t; ll_called_funcs: IdSet.t }
type ll_env_t = ll_func_info_t Env.t
type ll_subst_env_t = id_t Env.t

let lift_all top_code =
    let globals = ref (K_simple_ll.find_globals top_code IdSet.empty) in
    let is_global n = IdSet.mem n !globals in
    let ll_env = ref (Env.empty : ll_env_t) in
    let fold_fv0_ktyp_ t loc callb = () in
    let fold_fv0_kexp_ e callb =
        fold_kexp e callb; (* process all the sub-expressions in any case *)
        match e with
        (* some extra processing for each function *)
        | KDefFun {contents={kf_name; kf_loc}} ->
            let (uv, dv) = used_decl_by_kexp e in
            (* from the set of free variables we exclude global functions, values and types
               because they do not have to be put into a closure anyway *)
            let fv0 = IdSet.diff (IdSet.diff uv dv) !globals in
            let called_funcs = IdSet.fold (fun n called_funcs ->
                match (kinfo_ n kf_loc) with
                | KFun _ ->
                    if (is_global n) then called_funcs
                    else IdSet.add n called_funcs
                | _ -> called_funcs) uv IdSet.empty in
            let fv0 = IdSet.filter (fun fv -> match (kinfo_ fv kf_loc) with
                | KVal _ -> true
                | _ -> false) fv0 in
            ll_env := Env.add kf_name {ll_fvars=fv0; ll_declared_inside=dv; ll_called_funcs=called_funcs} !ll_env
        | _ -> ()
    in let fv0_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_fv0_ktyp_);
        kcb_fold_kexp = Some(fold_fv0_kexp_);
        kcb_fold_result = 0
    } in
    (* for each function, top-level or not, find the initial set of free variables,
       as well as the set of called functions *)
    let _ = List.iter (fun e -> fold_fv0_kexp_ e fv0_callb) top_code in

    (* now expand those sets. recursively add to the list of free variables
       all the free variables from the called functions
       (but not defined locally) *)
    let rec finalize_sets iters ll_all =
        let visited_funcs = ref IdSet.empty in
        let changed = ref false in
        let _ = if iters > 0 then () else raise_compile_err noloc
            "finalization of the free var sets takes too much iterations" in
        let rec update_fvars f =
            match (Env.find_opt f !ll_env) with
            | Some ll_info ->
                let { ll_fvars; ll_declared_inside; ll_called_funcs } = ll_info in
                if IdSet.mem f !visited_funcs then ll_fvars
                else
                    let _ = visited_funcs := IdSet.add f !visited_funcs in
                    let size0 = IdSet.cardinal ll_fvars in
                    let fvars = IdSet.fold (fun called_f fvars ->
                        let called_fvars = update_fvars called_f in
                        IdSet.union fvars called_fvars) ll_called_funcs ll_fvars in
                    let fvars = IdSet.diff fvars ll_declared_inside in
                    let size1 = IdSet.cardinal fvars in
                    let _ = if size1 = size0 then () else
                        (ll_info.ll_fvars <- fvars; changed := true) in
                    fvars
            | _ -> IdSet.empty
        in List.iter (fun f -> ignore (update_fvars f)) ll_all;
        if not !changed then (iters-1) else finalize_sets (iters - 1) (List.rev ll_all)
    in let iters0 = 10 in
    let _ = finalize_sets iters0 (List.rev (Env.fold (fun f _ ll_all -> f :: ll_all) !ll_env [])) in
    let all_fvars = Env.fold (fun _ ll_info all_fvars ->
        IdSet.union (ll_info.ll_fvars) all_fvars) !ll_env IdSet.empty in
    let all_mut_fvars = IdSet.filter (fun i -> is_mutable i (get_id_loc i)) all_fvars in
    (*
      for each mutable variable:
      - convert its type from 't' to 't*' (reference)
      - remove ValMutable flag, add ValImplicitDeref instead
    *)
    let _ = IdSet.iter (fun mut_fv ->
        let { kv_name; kv_typ; kv_flags; kv_scope; kv_loc } = get_kval mut_fv noloc in
        let new_kv_typ = KTypRef(kv_typ) in
        let new_kv_flags = ValImplicitDeref :: (List.filter (fun f -> f != ValMutable) kv_flags) in
        let new_kv = { kv_name; kv_cname=""; kv_typ=new_kv_typ; kv_flags=new_kv_flags; kv_scope; kv_loc } in
        set_idk_entry kv_name (KVal new_kv))
        all_mut_fvars in
    (* iterate through all the functions; for each function with
       free variables define a closure and add an extra parameter *)
    let fold_defcl_ktyp_ t loc callb = () in
    let fold_defcl_kexp_ e callb =
        fold_kexp e callb; (* process all the sub-expressions in any case *)
        match e with
        | KDefFun kf ->
            let {kf_name; kf_scope; kf_loc} = !kf in
            (match Env.find_opt kf_name !ll_env with
            | Some ll_info ->
                let fvars = ll_info.ll_fvars in
                if (IdSet.is_empty fvars) then ()
                else
                (let fvar_pairs_to_sort = IdSet.fold (fun fv fvars_to_sort ->
                    ((id2str fv), fv) :: fvars_to_sort) fvars [] in
                let fvar_pairs_sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fvar_pairs_to_sort in
                let fvars_final = List.map (fun (_, fv) -> fv) fvar_pairs_sorted in
                let kf_c_vt = gen_temp_idk ((id2prefix kf_name) ^ "_closure") in
                let fvars_wt = List.map (fun fv ->
                    let { kv_name; kv_typ; kv_flags; kv_scope; kv_loc } = get_kval fv kf_loc in
                    let new_fv = dup_idk fv in
                    let new_kv = { kv_name=new_fv; kv_cname=""; kv_typ; kv_flags; kv_scope=kf_scope; kv_loc=kf_loc } in
                    set_idk_entry new_fv (KVal new_kv); (new_fv, kv_typ)) fvars_final in
                let kcv = ref { kcv_name=kf_c_vt; kcv_cname=""; kcv_freevars=fvars_wt;
                    kcv_orig_freevars=fvars_final; kcv_scope=kf_scope; kcv_loc=kf_loc } in
                let kf_c_arg = gen_temp_idk "cv" in
                let kf_c_arg_kv = { kv_name=kf_c_arg; kv_cname=""; kv_typ=(KTypName kf_c_vt);
                                    kv_flags=[]; kv_scope=kf_scope; kv_loc=kf_loc} in
                let new_kf_closure = (kf_c_arg, kf_c_vt) in
                set_idk_entry kf_c_vt (KClosureVars kcv);
                set_idk_entry kf_c_arg (KVal kf_c_arg_kv);
                (* we do not add the closure parameter explicitly to kf_args;
                  instead, we put in kf_closure and then C generator will recognize and use it *)
                kf := { !kf with kf_closure=new_kf_closure })
            | _ -> ())
        | _ -> ()
    in let defcl_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_defcl_ktyp_);
        kcb_fold_kexp = Some(fold_defcl_kexp_);
        kcb_fold_result = 0
    } in
    (* recursively process each top-level expression; define the closures etc. *)
    let _ = List.iter (fun e -> fold_defcl_kexp_ e defcl_callb) top_code in

    let defined_so_far = ref IdSet.empty in
    let curr_clo = ref (noid, noid, noid) in
    let curr_subst_env = ref (Env.empty : ll_subst_env_t) in
    let curr_top_code = ref ([]: kexp_t list) in

    let rec walk_atom_n_lift_all a loc callb =
        match a with
        | Atom.Id (Id.Name _) -> a
        | Atom.Id n ->
            (match (Env.find_opt n !curr_subst_env) with
            | Some n2 -> Atom.Id n2
            | _ ->
                (match (kinfo n) with
                | KFun {contents={kf_flags}} ->
                    if List.mem FunConstr kf_flags then a
                    else raise_compile_err loc
                        (sprintf "for the function '%s' there is no corresponding closure" (id2str n))
                | _ -> a))
        | _ -> a
    and walk_ktyp_n_lift_all t loc callb = t
    and walk_kexp_n_lift_all e callb =
        match e with
        | KDefFun kf ->
            let { kf_name; kf_typ; kf_args; kf_body; kf_closure=(kf_cl_arg, kf_cl_vt); kf_scope; kf_loc } = !kf in
            let saved_dsf = !defined_so_far in
            let saved_clo = !curr_clo in
            let saved_subst_env = !curr_subst_env in
            let _ = curr_clo := (kf_name, kf_cl_arg, kf_cl_vt) in
            let _ = defined_so_far := List.fold_left (fun dsf arg -> IdSet.add arg dsf) !defined_so_far kf_args in

            let create_defclosure kf code =
                let {kf_name; kf_typ; kf_closure=(_, cl_vt); kf_scope; kf_loc} = !kf in
                let cl_name = dup_idk kf_name in
                let _ = curr_subst_env := Env.add kf_name cl_name !curr_subst_env in
                let _ = defined_so_far := IdSet.add cl_name !defined_so_far in
                let (_, orig_freevars) = get_closure_freevars cl_vt kf_loc in
                let cl_args = List.map (fun fv ->
                    if IdSet.mem fv !defined_so_far then ()
                    else raise_compile_err kf_loc
                        (sprintf "free variable '%s' of '%s' is not defined yet"
                        (id2str fv) (id2str kf_name));
                    walk_atom_n_lift_all (Atom.Id fv) kf_loc callb) orig_freevars
                    in
                let cl_typ = KTypClosure(kf_typ, cl_vt) in
                let make_cl = KExpMkClosure(kf_name, cl_args, (cl_typ, kf_loc)) in
                create_defval cl_name cl_typ [] (Some make_cl) code kf_scope kf_loc
                in

            let (prologue, def_cl_vt) =
                if kf_cl_vt = noid then ([], []) else
                    let kcv = match (kinfo_ kf_cl_vt kf_loc) with
                        | KClosureVars kcv -> kcv
                        | _ -> raise_compile_err kf_loc
                            (sprintf "closure type '%s' information is not valid (should be KClosureVars ...)"
                            (id2str kf_cl_vt))
                        in
                    let {kcv_freevars; kcv_orig_freevars} = !kcv in
                    (* for each free variable 'fv' we create a proxy 'val fv_proxy = kf_cl_arg.fv'
                      and add it to the function prologue. We also add the pair (fv, fv_proxy)
                      to the substitution dictionary. That is, we convert all the
                      accesses to the free variables *)
                    let (prologue, _) = List.fold_left2 (fun (prologue, idx) (fv, t) fv_orig ->
                        let _ = if IdSet.mem fv_orig !defined_so_far then () else
                            raise_compile_err kf_loc
                            (sprintf "free variable '%s' of function '%s' is not defined before the function body"
                            (id2str fv_orig) (id2str kf_name)) in
                        let fv_proxy = dup_idk fv in
                        let _ = curr_subst_env := Env.add fv_orig fv_proxy !curr_subst_env in
                        let _ = defined_so_far := IdSet.add fv_proxy !defined_so_far in
                        let kv_flags = match (kinfo_ fv_orig kf_loc) with | KVal {kv_flags} -> kv_flags | _ -> [] in
                        let get_fv = KExpMem (kf_cl_arg, idx, (t, kf_loc)) in
                        let new_prologue = create_defval fv_proxy t kv_flags (Some get_fv) prologue kf_scope kf_loc in
                        (new_prologue, idx+1)) ([], 0) kcv_freevars kcv_orig_freevars in
                    (* we also create a closure for each function with free variables
                        that is called from 'kf_name', but is not declared in 'kf_name' *)
                    let prologue = match Env.find_opt kf_name !ll_env with
                        | Some ({ll_declared_inside; ll_called_funcs}) ->
                            let called_fs = IdSet.diff ll_called_funcs ll_declared_inside in
                            IdSet.fold (fun called_f prologue ->
                                match (kinfo_ called_f kf_loc) with
                                | KFun called_kf ->
                                    let {kf_typ=called_ftyp; kf_closure=(_, called_cl_vt)} = !called_kf in
                                    if called_cl_vt = noid then prologue
                                    else create_defclosure called_kf prologue
                                | _ -> prologue) called_fs prologue
                        | _ -> raise_compile_err kf_loc
                            (sprintf "missing 'lambda lifting' information about function '%s'"
                            (id2str kf_name))
                        in
                    (prologue, [KDefClosureVars kcv])
                in
            (* add the generated prologue to the function body, then transform it alltogether *)
            let body_loc = get_kexp_loc kf_body in
            let body = code2kexp ((List.rev prologue) @ (kexp2code kf_body)) body_loc in
            let body = walk_kexp_n_lift_all body callb in
            defined_so_far := saved_dsf;
            curr_clo := saved_clo;
            curr_subst_env := saved_subst_env;
            curr_top_code := def_cl_vt @ !curr_top_code;
            kf := {!kf with kf_body=body};
            curr_top_code := (KDefFun kf) :: !curr_top_code;
            List.hd (create_defclosure kf [])
        | KDefVal(n, rhs, loc) ->
            let rhs = walk_kexp_n_lift_all rhs callb in
            defined_so_far := IdSet.add n !defined_so_far;
            KDefVal(n, rhs, loc)
        | KExpFor(idom_l, body, flags, loc) ->
            let idom_l = List.map (fun (i, dom_i) ->
                let dom_i = check_n_walk_dom dom_i loc callb in
                defined_so_far := IdSet.add i !defined_so_far;
                (i, dom_i)) idom_l in
            let body = walk_kexp_n_lift_all body callb in
            KExpFor(idom_l, body, flags, loc)
        | KExpMap(e_idom_ll, body, flags, ((etyp, eloc) as kctx)) ->
            let e_idom_ll = List.map (fun (e, idom_l) ->
                let e = walk_kexp_n_lift_all e callb in
                let idom_l = List.map (fun (i, dom_i) ->
                    let dom_i = check_n_walk_dom dom_i eloc callb in
                    defined_so_far := IdSet.add i !defined_so_far;
                    (i, dom_i)) idom_l
                in (e, idom_l)) e_idom_ll
            in let body = walk_kexp_n_lift_all body callb in
            KExpMap(e_idom_ll, body, flags, kctx)
        | KExpCall(f, args, ((_, loc) as kctx)) ->
            let args = List.map (fun a -> walk_atom_n_lift_all a loc callb) args in
            let (curr_f, curr_cl_arg, curr_cl_vt) = !curr_clo in
            if f = curr_f then
                let cl_arg =
                    if curr_cl_arg = noid then Atom.Lit LitNil
                    else Atom.Id curr_cl_arg
                in KExpCall(f, args @ [cl_arg], kctx)
            else
                (match (kinfo_ f loc) with
                | KFun {contents={kf_closure=(_, cl_vt)}} ->
                    if cl_vt = noid then
                        KExpCall(f, (args @ [(Atom.Lit LitNil)]), kctx)
                    else
                        KExpCall((check_n_walk_id f loc callb), args, kctx)
                | _ -> KExpCall((check_n_walk_id f loc callb), args, kctx))
        | _ -> walk_kexp e callb
    in let walk_n_lift_all_callb =
    {
        kcb_atom = Some(walk_atom_n_lift_all);
        kcb_typ = Some(walk_ktyp_n_lift_all);
        kcb_exp = Some(walk_kexp_n_lift_all)
    } in

    List.iter (fun e ->
        let e = walk_kexp_n_lift_all e walk_n_lift_all_callb in
        curr_top_code := e :: !curr_top_code) top_code;

    List.rev !curr_top_code
