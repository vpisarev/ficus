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

    * We analyze each function (which is a recursive algorithm that
      may be re-done more than once for certain functions) and
      see if the function has 'free variables', i.e. variables that
      are non-local and yet are non-global.
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
        fun bar(m: int) = n * m
        bar
      }

      is replaced with

      fun bar(m: int, c: bar_closure_t) {
        m*c->n
      }
      fun foo(n: int) {
        make_closure(bar, {n})
      }

    b) If the function does not have any 'free variables', we may still
      need a closure, i.e. we may need to represent this function as a pair,
      because in general when we pass a function as parameter to another function
      or store it as a value (essentially, we store a function pointer), the caller
      of that function does not know whether it needs free variables or not, so
      we need a consistent representation of functions that are called indirectly.
      But in this case we can have a pair (function, nil), i.e. we just use something like
      NULL pointer instead of a pointer to some real closure. So, the following code:

      fun foo(n: int) {
        fun bar(m: int) = n * m
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
          make_closure(bar, {n})
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
      exist anymore. The robust solution for this problem is to convert each 'var',
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
          make_closure(inc_me, {v})
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
              val (baz_cl_f, baz_cl_fv) = make_closure(baz, {c->n})
              baz_cl_f(m+1, baz_cl_fv)
          }
          fun baz( m: int, c:baz_closure_t* ) = m*c->n
          fun foo(n: int) = (make_closure(bar, {n}), make_closure(baz, {n})

          or it can be converted to

          // option 2: nested closure
          fun bar( m: int, c:bar_closure_t* ) {
              val (baz_cl_f, baz_cl_fv) = c->baz_cl
              baz_cl_f(m+1, baz_cl_fv)
          }
          fun baz( m: int, c:baz_closure_t* ) = m*c->n
          fun foo(n: int) = {
              val baz_cl = make_closure(baz, {n})
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
              val foo_nested_closure_data = {n}
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
    let fold_fv0_ktyp_ t callb = () in
    let fold_fv0_kexp_ e callb =
        fold_kexp e callb; (* process all the sub-expressions in any case *)
        match e with
        (* some extra processing for each function *)
        | KDefFun {contents={kf_name}} ->
            let (uv, dv) = used_decl_by_kexp e in
            (* from the set of free variables we exclude global functions, values and types
               because they do not have to be put into a closure anyway *)
            let fv0 = IdSet.diff (IdSet.diff uv dv) !globals in
            let called_funcs = IdSet.fold (fun n called_funcs ->
                match (kinfo n) with
                | KFun _ ->
                    if (is_global n) then called_funcs
                    else IdSet.add n called_funcs
                | _ -> called_funcs) uv IdSet.empty in
            let fv0 = IdSet.diff fv0 called_funcs in
            ll_env := Env.add kf_name {ll_fvars=fv0; ll_declared_inside=dv; ll_called_funcs=called_funcs} !ll_env)
        | _ -> ()
    in
    let fv0_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_fv0_ktyp_);
        kcb_fold_kexp = Some(fold_fv0_kexp_);
        kcb_fold_result = 0
    } in
    (* for each function, top-level or not, find the initial set of free variables,
       as well as the set of called functions *)
    let _ = List.iter (fun e -> fold_fv0_kexp_ e fv0_callb) top_code in
    let rec finalize_sets iters =
        let changed = ref false in
        if iters <= 0 then raise_compile_err noloc
            "too many iterations to produce the final sets of free variables for the lambda lifting; seems like there is bug in the code"
        else
        (Env.iter (fun fname ll_info ->
            let { ll_fvars; ll_declared_inside; ll_called_funcs } = ll_info in
            let size0 = IdSet.cardinal ll_fvars in
            let fvars = IdSet.fold (fun called_f fvars ->
                if called_f = fname then fvars else
                match (Env.find_opt called_f !ll_env) with
                | Some ll_called_info ->
                    IdSet.union fvars ll_called_info.ll_fvars
                | _ -> fvars) ll_called_funcs ll_fvars in
            let fvars = IdSet.diff fvars ll_declared_inside in
            let size1 = IdSet.cardinal fvars in
            ll_info.ll_fvars <- fvars;
            if size1 = size0 then () else changed := true) !ll_env;
        if !changed then finalize_sets (iters-1) else ()) in
    let _ = finalize_sets 1000 in
    let all_free_vars = Env.fold (fun _ info all_free_vars ->
        let {ll_fvars} = info in IdSet.union all_free_vars ll_fvars) !ll_env IdSet.empty in
    let declared_so_far = ref IdSet.empty in
    let curr_subst_env = ref (Env.empty : ll_subst_env_t) in
    let curr_top_code = ref ([]: kexp_t list) in
    let add_to_top e = curr_top_code := (List.rev (kexp2code e)) @ !curr_top_code in

    let rec walk_atom_n_lift_all a callb =
        match a with
        | Atom.Id n ->
            (match (Env.find_opt n !curr_subst_env) with
            | Some n2 -> Atom.Id n2
            | _ -> a)
        | _ -> a
    and walk_ktyp_n_lift_all t callb = t
    and walk_kexp_n_lift_all e callb =
        match e with
        | KDefFun kf ->
            let {kf_name; kf_typ; kf_args; kf_body; kf_closure; kf_loc} = !kf in
            let { ll_fvars; ll_declared_inside; ll_called_funcs } =
                match (Env.find_opt kf_name !ll_env) with
                | Some info -> info
                | _ -> raise_compile_err kf_loc
                    (sprintf "information about function '%s' is missing somehow" (id2str kf_name))
            in
            (*
            if the
            *)


            let new_body = walk_kexp_n_lift kf_body callb in
            let _ = kf := {!kf with kf_body=new_body} in
            if (is_global kf_name) then e
            else
                if not (can_lift_fun kf) then e
                else add_to_globals_and_lift kf_name e kf_loc
        | _ -> walk_kexp e callb
    in let walk_n_lift_callb =
    {
        kcb_atom = None;
        kcb_typ = Some(walk_ktyp_n_lift);
        kcb_exp = Some(walk_kexp_n_lift)
    } in

    let process top_code =
        new_top_code := [];
        List.iter (fun e ->
            let new_e = walk_kexp_n_lift e walk_n_lift_callb in
            match new_e with
            | KExpNop _ -> ()
            | _ -> new_top_code := new_e :: !new_top_code) top_code;
        List.rev !new_top_code in

    let top_code = process top_code in
