/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Performs "constant folding" + "dealiasing" optimization step of K-form:
    * `1+2` is replaced with `3` etc. (const1 op const2 => result_const)
    * `a+0, a-0, a*1, a/1` is replaced with `a`
    * `a-a, a*0, 0*a` is replaced with `0`
    * `a & true, true & a, a | false, false | a` is replaced with a
    * `a & false, false & a` is replaced with false
    * branches with constant conditions are expanded or eliminated:
      `if true {a} else {b}` is replaced with `a`,
      `if false {a} else {b}` is replaced with `b`.
    * trivial definitions are eliminated.
      That is, if the algorithm finds the definitions that look like
        * `val a@34535=b@795`, where `b@795` is immutable value,
        * or `val a@34535=3.1415`,
      it removes them and the replaces `a@34535` with `b@795` or `3.1415`,
      respectively, everywhere in the code. With a important exception:
      when a global user-defined value is assigned a temporary value,
      this temporary value may be eliminated, but not the user-defined value.
*/

from Ast import *
from K_form import *
import K_pp
import Math, Map

type aclass_t =
    | ConstZero
    | ConstInt: int64
    | ConstFloat: double
    | ConstBool: bool
    | ConstString: string
    | NonConst

fun string(a: aclass_t) =
    match a {
    | ConstZero => "Zero"
    | ConstInt i => f"ConstInt({i}i)"
    | ConstFloat f => f"ConstInt({f})"
    | ConstBool b => f"ConstBool({b})"
    | ConstString s => f"ConstString({s})"
    | NonConst => "NonConst"
    }

fun classify_atom(a: atom_t, z: bool): aclass_t =
    match a {
    | AtomLit la =>
        match la {
        | KLitInt x => if z && x == 0L { ConstZero } else { ConstInt(x) }
        | KLitSInt(_, x) => if z && x == 0L { ConstZero } else { ConstInt(x) }
        | KLitUInt(_, x) => if z && x == 0UL { ConstZero } else { ConstInt(int64(x)) }
        | KLitFloat(_, x) => if z && x == 0. { ConstZero } else { ConstFloat(x) }
        | KLitBool x => if z && x == false { ConstZero } else { ConstBool(x) }
        | KLitChar x => ConstString(string(x))
        | KLitString x => if z && x == "" { ConstZero } else { ConstString(x) }
        | KLitNil _ => NonConst
        }
    | AtomId _ => NonConst
    }

fun retain_atom(ac: aclass_t, a: atom_t, at: ktyp_t) =
    match ac {
    | NonConst => (None, Some((a, at)))
    | _ => (Some(ac), None)
    }

/* cfold_bop, cfold_uop, cfold_cast produce a pair of options:
   (const_option, atom_n_type_option) and pass it to finalize_cfold_result
   that returns a single option:
        * Some(const_fold_result_exp) - successfully folded.
          The caller function replaces the original expression with the
          provided result.
        * None - no folding has been done; retain the original expression as-is
   if const_option is Some(...:aclass_t) then it takes the preference,
   then we output (if possible) the computed atom of the specified type (res_t).
   otherwise if the second option is Some((atom, atom_type)) then
   we output either the original atom, or the casting expression to res_t. */
fun finalize_cfold_result(c_opt: aclass_t?, at_opt: (atom_t, ktyp_t)?, res_t: ktyp_t, loc: loc_t)
{
    fun mk_some_lit_atom(l: klit_t) = Some(KExpAtom(AtomLit(l), (res_t, loc)))

    match (c_opt, at_opt) {
    | (Some c, _) =>
        match c {
        | ConstZero =>
            match res_t {
            | KTypInt => mk_some_lit_atom(KLitInt(0L))
            | KTypSInt b => mk_some_lit_atom(KLitSInt(b, 0L))
            | KTypUInt b => mk_some_lit_atom(KLitUInt(b, 0UL))
            | KTypFloat b => mk_some_lit_atom(KLitFloat(b, 0.))
            | KTypBool => mk_some_lit_atom(KLitBool(false))
            | KTypString => mk_some_lit_atom(KLitString(""))
            | _ => None
            }
        | ConstInt x =>
            match res_t {
            | KTypInt => mk_some_lit_atom(KLitInt(x))
            | KTypSInt b => mk_some_lit_atom(KLitSInt(b, x))
            | KTypUInt b => mk_some_lit_atom(KLitUInt(b, uint64(x)))
            | KTypFloat b => mk_some_lit_atom(KLitFloat(b, double(x)))
            | KTypBool => mk_some_lit_atom(KLitBool(x != 0L))
            | KTypString => mk_some_lit_atom(KLitString(string(x)))
            | _ => None
            }
        | ConstFloat x =>
            match res_t {
            | KTypInt => mk_some_lit_atom(KLitInt(int64(x)))
            | KTypSInt b => mk_some_lit_atom(KLitSInt(b, int64(x)))
            | KTypUInt b => mk_some_lit_atom(KLitUInt(b, uint64(x)))
            | KTypFloat b => mk_some_lit_atom(KLitFloat(b, x))
            | KTypBool => mk_some_lit_atom(KLitBool(x != 0.))
            | KTypString => mk_some_lit_atom(KLitString(string(x)))
            | _ => None
            }
        | ConstBool x =>
            match res_t {
            | KTypInt => mk_some_lit_atom(KLitInt(int64(x)))
            | KTypSInt b => mk_some_lit_atom(KLitSInt(b, int64(x)))
            | KTypUInt b => mk_some_lit_atom(KLitUInt(b, uint64(x)))
            | KTypFloat b => mk_some_lit_atom(KLitFloat(b, double(x)))
            | KTypBool => mk_some_lit_atom(KLitBool(x))
            | KTypString => mk_some_lit_atom(KLitString(string(x)))
            | _ => None
            }
        | ConstString x =>
            match res_t {
            | KTypString => mk_some_lit_atom(KLitString(x))
            | _ => None
            }
        | _ => None
        }
    | (_, Some ((a, at))) =>
        if at == res_t { Some(KExpAtom(a, (at, loc))) }
        else { Some(KExpCast(a, res_t, loc)) }
    | _ => None
    }
}

fun cfold_bop(bop: binary_t, a: atom_t, b: atom_t, res_t: ktyp_t, loc: loc_t)
{
    val at = get_atom_ktyp(a, loc)
    val bt = get_atom_ktyp(b, loc)
    val z = match bop { | OpCmp _ | OpPow => false | _ => true }
    val ac = classify_atom(a, z)
    val bc = classify_atom(b, z)
    fun retain_a() = retain_atom(ac, a, at)
    fun retain_b() = retain_atom(bc, b, bt)

    /* the type checker has checked that the combination of 'a' and 'b' types is valid
        and it also computed the output type (passed here as 'res_t'), so
        we just rely on the type checker and avoid extra checks here */
    val (c_opt, a_opt) =
    match (bop, ac, bc) {
    | (_, NonConst, NonConst) =>
        if a == b {
            match bop {
            | OpSub | OpBitwiseXor => (Some(ConstZero), None)
            /* OpDiv and OpMod are skipped because there can be a=b=0 */
            | OpBitwiseAnd | OpBitwiseOr => retain_a()
            | OpCmp(CmpEQ) | OpCmp(CmpLE) | OpCmp(CmpGE) => (Some(ConstBool(true)), None)
            | OpCmp(CmpNE) | OpCmp(CmpLT) | OpCmp(CmpGT) => (Some(ConstBool(false)), None)
            | _ => (None, None)
            }
        } else {
            (None, None)
        }
    | (OpAdd, ConstZero, _) => retain_b()
    | (OpAdd, _, ConstZero) => retain_a()
    | (OpAdd, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia + ib)), None)
    | (OpAdd, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(ia + fb)), None)
    | (OpAdd, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(fa + ib)), None)
    | (OpAdd, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(fa + fb)), None)
    | (OpAdd, ConstString sa, ConstString sb) => (Some(ConstString(sa + sb)), None)
    | (OpSub, ConstZero, ConstZero) => (Some(ac), None)
    | (OpSub, ConstZero, ConstInt x) => (Some(ConstInt(-x)), None)
    | (OpSub, ConstZero, ConstFloat x) => (Some(ConstFloat(-x)), None)
    | (OpSub, _, ConstZero) => retain_a()
    | (OpSub, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia - ib)), None)
    | (OpSub, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(ia - fb)), None)
    | (OpSub, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(fa - ib)), None)
    | (OpSub, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(fa - fb)), None)
    | (OpMul, ConstZero, _) => (Some(ConstZero), None)
    | (OpMul, _, ConstZero) => (Some(ConstZero), None)
    | (OpMul, ConstInt (1L), _) => retain_b()
    | (OpMul, ConstFloat (1.), _) => retain_b()
    | (OpMul, _, ConstInt (1L)) => retain_a()
    | (OpMul, _, ConstFloat (1.)) => retain_a()
    | (OpMul, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia * ib)), None)
    | (OpMul, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(ia * fb)), None)
    | (OpMul, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(fa * ib)), None)
    | (OpMul, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(fa * fb)), None)
    | (OpDiv, _, ConstZero) | (OpMod, _, ConstZero) =>
        if !is_ktyp_integer(at, true) { (None, None) }
        else { throw compile_err(loc, "division by constant 0") }
    | (OpDiv, _, ConstInt (1L)) => retain_a()
    | (OpDiv, _, ConstFloat (1.)) => retain_a()
    | (OpDiv, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia / ib)), None)
    | (OpDiv, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(ia / fb)), None)
    | (OpDiv, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(fa / ib)), None)
    | (OpDiv, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(fa / fb)), None)
    | (OpMod, ConstInt ia, ConstInt ib) => (Some(ConstInt((ia % ib))), None)
    | (OpMod, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(double(ia) % fb)), None)
    | (OpMod, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(fa % double(ib))), None)
    | (OpMod, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(fa % fb)), None)
    | (OpBitwiseAnd, ConstZero, _) => (Some(ConstZero), None)
    | (OpBitwiseAnd, _, ConstZero) => (Some(ConstZero), None)
    | (OpBitwiseAnd, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia & ib)), None)
    | (OpBitwiseAnd, ConstBool ba, ConstBool bb) => (Some(ConstBool(ba & bb)), None)
    | (OpBitwiseOr, ConstZero, _) => retain_b()
    | (OpBitwiseOr, _, ConstZero) => retain_a()
    | (OpBitwiseOr, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia | ib)), None)
    | (OpBitwiseOr, ConstBool ba, ConstBool bb) => (Some(ConstBool(ba | bb)), None)
    | (OpBitwiseXor, ConstZero, _) => retain_b()
    | (OpBitwiseXor, _, ConstZero) => retain_a()
    | (OpBitwiseXor, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia ^ ib)), None)
    | (OpBitwiseXor, ConstBool ba, ConstBool bb) => (Some(ConstBool(ba ^ bb)), None)
    | (OpPow, ConstInt ia, ConstInt ib) =>
        if ib >= 0L {
            (Some(ConstInt(int64(round(Math.pow(double(ia), double(ib)))))), None)
        } else {
            throw compile_err(loc, "integer is raised to negative power; just use literal '0' instead")
        }
    | (OpPow, ConstInt ia, ConstFloat fb) => (Some(ConstFloat(Math.pow(double(ia), fb))), None)
    | (OpPow, ConstFloat fa, ConstInt ib) => (Some(ConstFloat(Math.pow(fa, double(ib)))), None)
    | (OpPow, ConstFloat fa, ConstFloat fb) => (Some(ConstFloat(Math.pow(fa, fb))), None)
    | (OpCmp(CmpEQ), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia == ib)), None)
    | (OpCmp(CmpNE), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia != ib)), None)
    | (OpCmp(CmpLT), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia < ib)), None)
    | (OpCmp(CmpGT), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia > ib)), None)
    | (OpCmp(CmpLE), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia <= ib)), None)
    | (OpCmp(CmpGE), ConstInt ia, ConstInt ib) => (Some(ConstBool(ia >= ib)), None)
    | (OpCmp(CmpEQ), ConstString sa, ConstString sb) => (Some(ConstBool(sa == sb)), None)
    | (OpCmp(CmpNE), ConstString sa, ConstString sb) => (Some(ConstBool(sa != sb)), None)
    | (OpCmp(CmpEQ), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa == fb)), None)
    | (OpCmp(CmpNE), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa != fb)), None)
    | (OpCmp(CmpLT), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa < fb)), None)
    | (OpCmp(CmpGT), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa > fb)), None)
    | (OpCmp(CmpLE), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa <= fb)), None)
    | (OpCmp(CmpGE), ConstFloat fa, ConstFloat fb) => (Some(ConstBool(fa >= fb)), None)
    | (OpCmp(CmpEQ), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba == bb)), None)
    | (OpCmp(CmpNE), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba != bb)), None)
    | (OpCmp(CmpLT), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba < bb)), None)
    | (OpCmp(CmpGT), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba > bb)), None)
    | (OpCmp(CmpLE), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba <= bb)), None)
    | (OpCmp(CmpGE), ConstBool ba, ConstBool bb) => (Some(ConstBool(ba >= bb)), None)
    | (OpShiftLeft, ConstZero, _) => (Some(ConstZero), None)
    | (OpShiftLeft, _, ConstZero) => retain_a()
    | (OpShiftLeft, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia << ib)), None)
    | (OpShiftRight, ConstZero, _) => (Some(ConstZero), None)
    | (OpShiftRight, _, ConstZero) => retain_a()
    | (OpShiftRight, ConstInt ia, ConstInt ib) => (Some(ConstInt(ia >> ib)), None)
    | _ => (None, None)
    }
    finalize_cfold_result(c_opt, a_opt, res_t, loc)
}

fun cfold_uop(uop: unary_t, a: atom_t, res_t: ktyp_t, loc: loc_t)
{
    val at = get_atom_ktyp(a, loc)
    val ac = classify_atom(a, false)
    fun retain_a() = retain_atom(ac, a, at)

    val (c_opt, a_opt) =
    match (uop, ac) {
    | (OpPlus, NonConst) => retain_a()
    | (OpPlus, ConstInt _) => retain_a()
    | (OpPlus, ConstFloat _) => retain_a()
    | (OpNegate, ConstInt x) => (Some(ConstInt(-x)), None)
    | (OpNegate, ConstFloat x) => (Some(ConstFloat(-x)), None)
    | (OpBitwiseNot, ConstInt x) => (Some(ConstInt(~x)), None)
    | (OpBitwiseNot, ConstBool x) => (Some(ConstBool(!x)), None)
    | (OpLogicNot, ConstBool x) => (Some(ConstBool(!x)), None)
    | _ => (None, None)
    }
    finalize_cfold_result(c_opt, a_opt, res_t, loc)
}

fun cfold_cast(a: atom_t, res_t: ktyp_t, loc: loc_t)
{
    val at = get_atom_ktyp(a, loc)
    val ac = classify_atom(a, false)
    val (c_opt, a_opt) = retain_atom(ac, a, at)
    finalize_cfold_result(c_opt, a_opt, res_t, loc)
}

type idamap_t = (id_t, atom_t) Map.t
type idalmap_t = (id_t, atom_t list) Map.t

fun print_subst_map(m: idamap_t, loc: loc_t) {
    println("subsitution map {")
    m.app(fun (n, a) {
        println(f"\t{idk2str(n, loc)}: ")
        K_pp.pp_atom(a, loc)
        println(";")
    })
    println("}")
}

fun cfold_dealias(kmods: kmodule_t list)
{
    var ida_map: idamap_t = Map.empty(cmp_id)
    var concat_map: idalmap_t = Map.empty(cmp_id)
    var mktup_map: idalmap_t = Map.empty(cmp_id)

    fun cfd_atom_(a: atom_t, loc: loc_t, callb: k_callb_t) =
        match a {
        | AtomId n =>
            match ida_map.find_opt(n) {
            | Some a2 => a2
            | _ => a
            }
        | _ => a
        }
    fun cfd_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun cfd_kexp_(e: kexp_t, callb: k_callb_t)
    {
        /* first, process all the sub-expressions; the only exception is KDefVal,
           which we handle separately */
        val e = match e { | KDefVal _ => e | _ => walk_kexp(e, callb) }
        match e {
        | KDefVal (n, rhs_e, loc) =>
            val rhs_e = cfd_kexp_(rhs_e, callb)
            val {kv_flags} = get_kval(n, loc)
            val n = match cfd_atom_(AtomId(n), loc, callb) {
                    | AtomId n2 => n2
                    | _ => n
                    }
            val e = KDefVal(n, rhs_e, loc)
            if !is_mutable(n, loc) {
                match rhs_e {
                | KExpAtom (a, (_, loc2)) =>
                    match a {
                    | AtomId n2 =>
                        /* in order to do a safe substitution, both values
                            must be immutable, otherwise the change may affect the semantics
                        */
                        if !is_mutable(n2, loc2) {
                            match (n, n2) {
                            /* if a temporary value is assigned to the user-defined value,
                               we'd better keep the user-specified name so
                               that the output code is cleaner (and sometimes enabling such subsitution may
                               cause problems with separate compilation of .c sources, when
                               the temporary value suddenly needs to be accessed from another module. */
                            | (IdVal _, IdTemp _) => e
                            | _ => ida_map = ida_map.add(n, AtomId(n2)); KExpNop(loc)
                            }
                        } else { e }
                    | AtomLit c => ida_map = ida_map.add(n, a); KExpNop(loc)
                    }
                | KExpIntrin (IntrinStrConcat, al, (_, loc)) when
                        kv_flags.val_flag_temp && all(for a <- al {!is_mutable_atom(a, loc)}) =>
                    concat_map = concat_map.add(n, al); e
                | KExpMkTuple (al, (_, loc)) when
                        kv_flags.val_flag_temp && all(for a <- al {!is_mutable_atom(a, loc)}) =>
                    mktup_map = mktup_map.add(n, al); e
                | _ => e
                }
            } else { e }
        | KExpIntrin(IntrinStrConcat, al, (res_t, loc)) =>
            // Some of the concatenated atoms (al) might be, in their turn,
            // be as well string concatenations. So we want to expand it all and make
            // a completely flat list - make it one big concatenation.
            // At the same time, we do constant folding when two constant strings are
            // concatentated together.
            fun try_cfold_str_concat(a: atom_t, res_al: atom_t list): atom_t list =
                match (a, res_al) {
                // the order s2 + s1 is correct here, since we operate on reversed lists
                | (AtomLit(KLitChar c1), AtomLit(KLitChar c2) :: rest) => AtomLit(KLitString(string(c2) + c1)) :: rest
                | (AtomLit(KLitString s1), AtomLit(KLitChar c2) :: rest) => AtomLit(KLitString(c2 + s1)) :: rest
                | (AtomLit(KLitChar c1), AtomLit(KLitString s2) :: rest) => AtomLit(KLitString(s2 + c1)) :: rest
                | (AtomLit(KLitString s1), AtomLit(KLitString s2) :: rest) => AtomLit(KLitString(s2 + s1)) :: rest
                | (AtomLit(KLitString("")), res_al) => res_al
                | (a, AtomLit(KLitString("")) :: rest) => a :: rest
                | _ => a :: res_al
                }

            val fold res_al = [] for a <- al {
                | AtomId n =>
                    match concat_map.find_opt(n) {
                    | Some(a :: rest) => rest.rev() + try_cfold_str_concat(a, res_al)
                    | _ => a :: res_al
                    }
                | _ => try_cfold_str_concat(a, res_al)
                }

            match res_al {
            | (a :: []) when (match get_atom_ktyp(a, loc) { | KTypString => true | _ => false }) =>
                KExpAtom(a, (res_t, loc))
            | _ => KExpIntrin(IntrinStrConcat, res_al.rev(), (res_t, loc))
            }
        | KExpBinary (bop, a, b, (res_t, loc)) =>
            match cfold_bop(bop, a, b, res_t, loc) { | Some new_e => new_e | _ => e }
        | KExpUnary (uop, a, (res_t, loc)) =>
            match cfold_uop(uop, a, res_t, loc) { | Some new_e => new_e | _ => e }
        | KExpCast (a, t, loc) =>
            match cfold_cast(a, t, loc) { | Some new_e => new_e | _ => e }
        | KExpMem (t_id, idx, (res_t, loc)) =>
            // (a0, a1, ... a(N-1)).i === ai
            match mktup_map.find_opt(t_id) {
            | Some al =>
                val n = al.length()
                if !(0 <= idx && idx < n) {
                    throw compile_err(loc, f"index ({idx}) is out of range [0, {n - 1}] during tuple access optimization")
                }
                KExpAtom(al.nth(idx), (res_t, loc))
            | _ => e
            }
        | KExpIf (c, then_e, else_e, kctx) =>
            // eliminate dead branches
            match c {
            | KExpAtom (AtomLit(KLitBool(true)), _) => then_e
            | KExpAtom (AtomLit(KLitBool(false)), _) => else_e
            | _ => KExpIf(c, then_e, else_e, kctx)
            }
        | KExpWhile (c, body, loc) =>
            match c {
            | KExpAtom (AtomLit(KLitBool(false)), _) => KExpNop(loc)
            | _ => e
            }
        /* we do not convert KExpDoWhile(body, false, loc)
           into a nested KExpSeq(), because then we will have to
           correctly transform the nested break and continue operators, if any.
           For now, leave it as-is */
        /*| KExpDoWhile (body, c, loc) =>
            match c {
            | KExpAtom (AtomLit(KLitBool(false)), _) => body
            | _ => e
            }*/
        | KExpMatch _ =>
            /* in general, the KExpMatch() operator has the following form
                if( {expressions11 ... actual_check11} &&
                    {expressions12 ... actual_check12} && ... )
                    action1
                else if( {expressions21 ... actual_check21} && ... )
                    action2
                ...
                else
                    // k-normalization step makes sure that there is always the 'else' branch.
                    actionN.

                If some of the actual_check(i,j) are constant "true" or "false",
                we can optimize the conditions a bit:

                * if some actual_check(i,j) === false,
                    then the remaining checks {expressions(i,k) ... actual_check(i,k)} for k > j can be removed;
                    the corresponding action(i) can probably be converted into a 'nop' (or 'nil').
                * if some actual_check(i,j) === true then
                    expressions(i,j) ... should be added to the next expressions(i,j+1).
                    if it was the last check in case, then the expressions should be added
                    to the action(i).
                    if there was just one check, i.e. we have
                    "...
                    else if( {expressions(i,1) ... true }) action(i)
                    ...", then we replace it with
                    "... else { expressions(i,1) ... action(i) }". The remaining cases are removed.
                    if i=1 then the whole KExpMatch is replaced with KExpSeq({expressions(1,1) ... action(1)}).
                Sounds rather complex, but the code is not much longer than this description.
            */
            match e {
            | KExpMatch (cases, (match_ktyp, match_loc) as kctx) =>
                fun process_case_checks(checks: kexp_t list, next_check_code: kcode_t,
                                        result_checks: kexp_t list) =
                    match checks {
                    | c :: other_checks =>
                        val code = next_check_code + kexp2code(c)
                        match code {
                        | [] => process_case_checks(other_checks, [], result_checks)
                        | _ =>
                            val actual_check = code.last()
                            match actual_check {
                            | KExpAtom (AtomLit(KLitBool(false)), _) =>
                                val loc = get_kexp_loc(actual_check)
                                val new_c = code2kexp(code, loc)
                                (false, [], (new_c :: result_checks).rev())
                            | KExpAtom (AtomLit(KLitBool(true)), _) =>
                                val code_wo_check = List.tl(code.rev()).rev()
                                process_case_checks(other_checks, code_wo_check, result_checks)
                            | _ =>
                                val loc = get_kexp_loc(actual_check)
                                val new_c = code2kexp(code, loc)
                                process_case_checks(other_checks, [], new_c :: result_checks)
                            }
                        }
                    | _ => (true, next_check_code, result_checks.rev())
                    }
                fun process_cases(cases: (kexp_t list, kexp_t) list,
                                  result: (kexp_t list, kexp_t) list) =
                    match cases {
                    | (checks, e) :: other_cases =>
                        if other_cases.empty() && !checks.empty() {
                            throw compile_err(match_loc, "the match does not have the else branch")
                        }
                        val (keep_action, action_extra_code, checks) = process_case_checks(checks, [], [])
                        val eloc = get_kexp_loc(e)
                        val new_action =
                            if keep_action {
                                code2kexp(action_extra_code + (e :: []), eloc)
                            } else {
                                match match_ktyp {
                                | KTypVoid => KExpNop(eloc)
                                | _ => KExpAtom(AtomLit(KLitNil(match_ktyp)), (match_ktyp, eloc))
                                }
                            }
                        match (keep_action, checks) {
                        | (false, KExpAtom (AtomLit(KLitBool(false)), _) :: []) =>
                            // drop the case completely, because the check is trivial (==FALSE)
                            process_cases(other_cases, result)
                        | (true, []) =>
                            // no checks; it means that we have 'else' or the new 'else' case;
                            // we can skip the rest
                            (([], new_action) :: result).rev()
                        | _ => process_cases(other_cases, (checks, new_action) :: result)
                        }
                    | _ => result.rev()
                    }

                val cases = process_cases(cases, [])
                match cases {
                | ([], else_action) :: [] => else_action
                | _ => KExpMatch(cases, kctx)
                }
            | _ => e
            }
        | _ => e
        }
    }

    val cfd_callb = k_callb_t
    {
        kcb_atom=Some(cfd_atom_),
        kcb_ktyp=Some(cfd_ktyp_),
        kcb_kexp=Some(cfd_kexp_)
    }
    [: for km <- kmods {
        val {km_top=top_code} = km
        val top_code = [: for e <- top_code { cfd_kexp_(e, cfd_callb) } :]
        val top_code = [: for e <- top_code { cfd_kexp_(e, cfd_callb) } :]
        km.{km_top=top_code}
    } :]
}
