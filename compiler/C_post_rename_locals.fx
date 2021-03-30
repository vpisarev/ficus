/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*  rename the local variables in each generated C function so that
    the function body does not change if neither its .fx source was changed
    nor the source of inline functions it calls. It's a crucial feature
    for separate compilation
*/

from Ast import *
from K_form import *
from C_form import *

import Hashmap

fun cmp_int(a: int, b: int): int = a <=> b
type int_map_t = (int, int) Hashmap.t
fun empty_int_map(size0: int) = Hashmap.empty(size0, 0, -1, hash)

fun rename_locals(cmods: cmodule_t list)
{
    var global_prefix_hash: int_map_t = empty_int_map(256)
    var prefix_hash = empty_int_map(256)

    fun gen_cname(n: id_t) {
        val prefix= match n {
                    | IdName i => i
                    | IdVal (i, j) => i
                    | IdTemp (i, j) => i
                    }
        val idx = prefix_hash.find_idx_or_insert(prefix)
        val j1 = prefix_hash.r->table[idx].data + 1
        prefix_hash.r->table[idx].data = j1
        val prefix = dynvec_get(all_strings, prefix)
        f"{prefix}_{j1}"
    }

    fun gen_cval_cname(n: id_t, loc: loc_t) =
        match n {
        | IdName _ => {}
        | _ =>
            match cinfo_(n, loc) {
            | CVal cv =>
                val {cv_cname} = cv
                if cv_cname == "" {
                    val new_cname = gen_cname(n)
                    set_idc_entry(n, CVal(cv.{cv_cname=new_cname}))
                }
            | _ => throw compile_err(loc, f"invalid id info for '{idc2str(n, loc)}'; it must be CVal")
            }
        }

    fun rename_cstmt(s: cstmt_t, callb: c_fold_callb_t) =
        match s {
        | CDefVal(t, n, e_opt, loc) when
            (match n { | IdName _ => false | _ => true}) =>
            match e_opt {
            | Some e => fold_cexp(e, callb)
            | _ => {}
            }
            gen_cval_cname(n, loc)
        | CStmtLabel (n, loc) when
            (match n { | IdName _ => false | _ => true}) =>
            match cinfo_(n, loc) {
            | CLabel cl =>
                val {cl_cname} = cl
                if cl_cname == "" {
                    val new_cname = gen_cname(n)
                    set_idc_entry(n, CLabel(cl.{cl_cname=new_cname}))
                }
            | _ => throw compile_err(loc, f"invalid id info for '{idc2str(n, loc)}'; it must be CLabel")
            }
        | CStmtFor (Some t, decls, _, _, _, loc) =>
            for e <- decls {
                match e {
                | CExpBinary (COpAssign, CExpIdent (n, _), _, _) => gen_cval_cname(n, loc)
                | _ => {}
                }
            }
            fold_cstmt(s, callb)
        | CDefFun cf =>
            val {cf_args, cf_loc} = *cf
            val saved_hash = prefix_hash
            prefix_hash = global_prefix_hash.copy()
            for (argname, _, _) <- cf_args {
                gen_cval_cname(argname, cf_loc)
            }
            fold_cstmt(s, callb)
            prefix_hash = saved_hash
        | _ => fold_cstmt(s, callb)
        }

    val rename_callb = c_fold_callb_t {
        ccb_fold_ident=None,
        ccb_fold_typ=None,
        ccb_fold_exp=None,
        ccb_fold_stmt=Some(rename_cstmt)
    }

    // pass 1. collect hash of global names that still do not have cname assigned
    // (normally there should be no such names)
    for cmod <- cmods {
        val {cmod_ccode} = cmod
        for s <- cmod_ccode {
            | CDefVal(_, n, _, loc) when
                (match n { | IdName _ => false | _ => true}) =>
                gen_cval_cname(n, loc)
            | _ => {}
        }
    }
    global_prefix_hash = prefix_hash

    // pass 2. rename local variables in each function in each module.
    // use the global prefix hash as a starting point for each function.
    // this way we make sure that local value do not interfere with global values
    for cmod <- cmods {
        val {cmod_ccode} = cmod
        prefix_hash = global_prefix_hash.copy()
        for s <- cmod_ccode {
            rename_cstmt(s, rename_callb)
        }
    }
    cmods
}
