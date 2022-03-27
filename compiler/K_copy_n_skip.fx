/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    The module copies inline functions, instances of generic functions and
    instances of generic types from the modules where they are defined
    to the modules where they are used.

    It also determines whether some of the K-normalized modules can be
    excluded from K-form optimization and C-code generation stages.

    These steps help to achieve a fast incremental compilation.

    For example, consider a module Hashmap, which defines generic
    Hashmap.t type - the hash table. Somewhere in user's code
    the generic Hashmap.t type is instantiated, probably with user-defined
    key and value types, but not necessarily. At once, some of the generic
    functions/methods that operate on this hash table are instantiated as well.

    Initially, all the instances are placed into the original module where
    the corresponding generic types/functions are defined (let's call it
    the parent module). And this is a very good approach, because we make sure that
    we have exactly once instance of a generic type/function with a certain
    set of arguments (concrete types). We type-check and k-normalize just
    this once instance. When we are in the global optimization mode,
    keeping the instances in the parent module during all further
    transformations is also the most efficient approach.

    But suppose that we want to implement very efficient incremental
    compilation mode, where only a small part of a program is recompiled
    after some local changes in a few user-defined modules. The natural way to
    do it is to cache K-form, and also check if any dependencies of each
    particular module have been modified. Suppose that  we dumped K-form of
    each module Mj shortly after K-normalization to a file. Let's call the K-form KF0'(Mj),
    where KF0(Mj) would mean K-form right after K-normalization of Mj and KF0'(Mj) is
    the form after a few initial steps, like dead code elimination.

    In theory we would like to skip K-form optimization and C code generation
    steps for Mj if

        KF0'_current(Mj)) == KF0'_previous(Mj) &&
        all(dep <- deps(Mj) {KF0'_current(dep) == KF0'_previous(dep)}) &&
        ...

    (where ... means a recursive check of not just direct dependencies of
    Mj, but also the dependencies of dependencies etc.).

    Now, suppose that we have 2 modules, A and B, both of which use Hashmap.
    We edited one of the modules (e.g. A), and, as a result of those
    changes, another instance of Hashmap.t is created, together with
    the corresponding methods.

    Because all the instances of Hashmap.t and its methods are placed into
    Hashmap module, A ~> A' yields Hashmap ~ Hashmap'. It means that
    after we edited just A, but need to rebuild all 3 modules:
    A (because we edited it), Hashmap (because a new instance of the
    generic type and several functions appeared there, so it was implicitly
    changed as well) and B (because one of its dependencies, Hashmap,
    has been changed). That is, the existing scheme is completely
    incompatible with the idea of fast incremental compilation.

    To solve this problem, we copy (not move) instances of generic types
    and generic functions, as well as inline functions, from the parent
    module to the module where the functionality is used. After that
    we run another round of dead code elimination, which will likely
    remove most of the generic type/function instances from the parent module,
    since normally they are not used in the module where they are defined
    (at least in the case of the standard library), just defined.

    In the above scenario with 3 modules: A, B and Hashmap, after applying
    "copying" and "dead code elimiation" steps only A will have changes.
    Because Hashmap.t consists of only generic function/types,
    it will become empty after those transformations. So we will
    have to recompile only A. B and Hashmap will be skipped as they
    are not modified.

    Of course, some of the modules that we decided to skip may have some
    non-generic functionality as well, and so they will stay non-empty after
    copying some functions from them and running another round of dead code
    elimination. How do we process them? We apply the following trick -
    the bodies of all remaining functions are converted to dummy @ccode {}.
    Then we proceed with the modules as usual through all the K-form
    optimization stages and then C-code generation stages. After such
    function body subsitution the modules will be processed really fast.
    We do not use the produced C code. We do not pretty-print it to
    a .c file. We just discard it and use the previously generated .c code.

    Now, what if we have some module C that imports A and also uses
    some of the same instances of Hashmap.t as A? C may call some functions
    in A and pass there those instances. Yet, after K_copy we will have 2
    separate copies of this Hashmap.t instance, one in A and another in C.

    How to solve the data structures compatiblity problem?

    This is solved by adding kvar_proto to kdefvariant_t type.
    A similar kt_proto field is added to kdeftype_t (it's useful only
    for records, because other types are anonymous and thus do not
    depend on the location where they are defined). For functions
    nothing like that is needed because it's fine to have several
    exactly the same functions in different modules.
    In all copies of the same instance of a generic type the field
    kvar_proto/kt_proto is be set to the id of the original copy from Hashmap.t.
    The dead code elimination step removes unused definitions from the code,
    but retains entries in the symbol table. So the compiler can always
    check that the two types are actually the same type. Besides,
    name mangling (K_mangle.fx) uses kvar_proto/kt_proto to generate
    the proper symbolic names. After that C code generator will produce
    exactly the same type definitions for different copies of the same
    instance of a generic type. And then C compiler will generate
    correct code. Even a C++ compiler with its own name mangling
    and extra parameter types verification will not complain.

    Why do we copy not just instances of generic types/functions,
    but also inline functions? Because when we decide to skip
    processing a certain module, we convert all its non-inline
    function bodies to dummy '@ccode {}', as mentioned above.
    This approach works well for all but inline functions.
    They need to keep their original bodies. Why then not just
    retain them as-is in the modules where they are defined?
    This is because we want incremental compilation mechanism be
    as stable as possible. That is, the produced C code should
    not depend (or depend very little) on whether we decided
    to re-process some module or not. The current inline function
    expansion algorithm, quite agressive one, attempts to inline
    any function call, no matter if the caller and the callee are
    in the same module or not, no matter if the called function is
    inline or not. Of course, it's poorly compatible with the idea of
    converting some functions' bodies to '@ccode {}'.

    The natural way to make incremental complilation more robust is
    to prohibit inline expansion between modules. This is basically what
    C/C++ compilers do - unless some global optimization option is used,
    they process each .c/.cpp file independently. If one wants a small
    function to be inlined in multiple modules, it's put in a header file,
    which after the preprocessor step is embedded into each .c/.cpp file
    that includes the header. That is, we get several copies of the inline
    function. Same approach is used here. As a bonus, we not only make
    the incremental compilation not only more robust, we allow the inline
    function expansion step to be run in parallel on different modules.
*/

from Ast import *
from K_form import *
import K_inline, K_pp
import Map, Set, Hashmap, Hashset

type subst_map_t = (id_t, id_t) Hashmap.t

fun copy_some(kmods: kmodule_t list)
{
    val toposort_idx = array(size(all_modules), 123456789)
    for {km_idx, km_toposort_idx} <- kmods {
        toposort_idx[km_idx] = km_toposort_idx
    }

    // step 1. Scan all the modules, collect instances of
    // generic types and functions, as well as inline functions.
    // Note that we only need data types and functions accessible from
    // other modules, so we need to scan only the top level of each module.
    // The index is used for the further sorting to put
    // the copied functionality in more or less stable order, which
    // is similar to the order of defined functionality.
    var all_copied: (id_t, (int, kexp_t)) list = []
    var idx = 0
    for km <- kmods {
        val {km_top} = km
        for e <- km_top {
            | KDefFun (ref {kf_name, kf_flags})
                when kf_flags.fun_flag_instance || kf_flags.fun_flag_inline =>
                all_copied = (kf_name, (idx, e)) :: all_copied
                idx += 1
            | KDefTyp (ref {kt_name, kt_targs}) =>
                all_copied = (kt_name, (idx, e)) :: all_copied
                idx += 1
            | KDefVariant (ref {kvar_name, kvar_flags}) =>
                all_copied = (kvar_name, (idx, e)) :: all_copied
                idx += 1
            | KDefVal (n, _, loc) =>
                val kv = get_kval(n, loc)
                if kv.kv_flags.val_flag_instance || kv.kv_flags.val_flag_ctor > 0 {
                    all_copied = (n, (idx, e)) :: all_copied
                    idx += 1
                }
            // maybe should also consider interfaces?
            | _ => {}
        }
    }
    val all_copied_hash = Hashmap.from_list(noid, (0, KExpNop(noloc)), all_copied)
    //println(f"# of copied definitions: {all_copied.length()}")

    // step 2. For each symbol from the constructed list compute the set of its
    // direct dependencies from the same set
    val all_copied_set = empty_id_hashset(1)
    for (n, _) <- all_copied { all_copied_set.add(n) }
    val idset0 = empty_id_hashset(1)
    var all_deps = Hashmap.empty(1024, noid, idset0)
    for (n, (_, e)) <- all_copied {
        val deps = used_by([:: e], 16)
        deps.intersect(all_copied_set)
        all_deps.add(n, deps)
    }

    // step 3. Compute the final set of dependencies as a transitive closure:
    // if a depends on b and b depends on c then a depends on c.
    val iters0 = 10
    val _ = calc_sets_closure(iters0, [:: for (n, _) <- all_copied {n} ], all_deps)

    // step 4. This step is the final one. Actually copy into each module
    // the functionality (from the above defined category:
    // instances of generic types and functions, inline functions)
    // it uses, directly or indirectly.
    // The step is quite complex, so we split it into sub-items.
    val all_copied_code = [:: for km <- kmods {
        val {km_top, km_idx, km_toposort_idx} = km

        // 4.1. find all ids directly used by the currently processed module
        val u_ids = used_by(km_top, 16)
        u_ids.intersect(all_copied_set)
        //print_id_hashset(f"ids used by {pp(get_module_name(km_idx))} directly", u_ids)

        // 4.2. find all ids from the other modules, directly or indirectly
        // used by the processed modules.
        val all_u_ids = empty_id_hashset(1)
        u_ids.app(fun (n) {
            if toposort_idx[n.m] < km_toposort_idx {
                all_u_ids.add(n)
                match all_deps.find_opt(n) {
                | Some(deps) =>
                    //deps.app(fun (nj) { if nj.m < km_idx {all_u_ids.add(nj)} })
                    deps.app(fun (nj) { if toposort_idx[nj.m] < km_toposort_idx {all_u_ids.add(nj)} })
                | _ => {}
                }
            }})
        //print_id_hashset(f"ids used by {pp(get_module_name(km_idx))} directly or indirectly", all_u_ids)

        // 4.3. based on the just computed set we extract all the code
        // to copy from other modules to the currently processed module.
        var copied_code = []
        all_u_ids.app(fun (n) {
                match all_copied_hash.find_opt(n) {
                | Some((idx, e)) => copied_code = (idx, e) :: copied_code
                | _ => {}
                }
            })

        // 4.3.1. this is where we will make use of idx's.
        // Bring the extracted code to more or less stable order and
        // then discard indices
        copied_code = copied_code.sort(fun ((idx1, _), (idx2, _)) {idx1 < idx2})
        val copied_code = [:: for (_, e) <- copied_code {e} ]

        //K_pp.pp_top(f"code copied to '{pp(get_module_name(km_idx))}' before rename: {copied_code.length()} definitions", copied_code)

        // 4.4. the code to copy from all other modules into
        // the currently processed module has been formed.
        // Now we need to create a copy of the code and at once rename
        // all the external and locally defined names in the code.
        val empty_subst0 = Hashmap.empty(1, noid, AtomId(noid))
        val (copied_code, subst_map) = K_inline.subst_names(km_idx,
                    code2kexp(copied_code, noloc), empty_subst0, true)
        val copied_code = kexp2code(copied_code)

        // 4.5. mark the copied values as private to avoid linker errors/warnings about duplicated symbols
        for e <- copied_code {
            | KDefVal(n, e, loc) =>
                val kv = get_kval(n, loc)
                val {kv_flags} = kv
                val new_kv_flags = kv_flags.{val_flag_private=true, val_flag_global=[]}
                set_idk_entry(n, KVal(kv.{kv_flags=new_kv_flags}))
            | _ => {}
            }
        (copied_code, subst_map)
    }]

    // step 5. Append the copied code to the destination modules' code, update all the references to the copied code
    [:: for km <- kmods, (copied_code, subst_map) <- all_copied_code {
        val {km_top, km_idx} = km

        //K_pp.pp_top(f"code copied to '{pp(get_module_name(km_idx))}' after rename: {copied_code.length()} definitions", copied_code)

        // update all the references to the copied code in the existing module
        val (new_top, _) = K_inline.subst_names(km_idx, code2kexp(km_top, noloc), subst_map, false)
        val new_top = kexp2code(new_top)

        //K_pp.pp_top(f"existing '{pp(get_module_name(km_idx))}' code after rename: {copied_code.length()} definitions", new_top)

        // ok, now just prepend the updated code from the module with the imported functionality
        km.{km_top = copied_code + new_top}
    }]
}
