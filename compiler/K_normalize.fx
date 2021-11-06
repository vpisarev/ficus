/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Converts the Abstract Syntax Tree (Ast.fx) into K-form (KForm.fx).

    For now only the global compilation mode is supported, i.e.
    the code from all the modules, used in the program,
    is put into one global list of definitions and top-level expressions.
    Since the modules are processed in the topological order, the output
    is correct.
*/
from Ast import *
from K_form import *
import Ast_typecheck, Ast_pp
import File, Set

fun typ2ktyp(t: typ_t, loc: loc_t): ktyp_t
{
    var id_stack: id_t list = []
    fun typ2ktyp_(t: typ_t): ktyp_t {
        val t = deref_typ(t)
        match t {
        | TypVar (ref Some(t)) => typ2ktyp_(t)
        | TypVar _ => throw compile_err(loc,
            "undefined type; use explicit type annotation")
        | TypInt => KTypInt
        | TypSInt(b) => KTypSInt(b)
        | TypUInt(b) => KTypUInt(b)
        | TypFloat(b) => KTypFloat(b)
        | TypString => KTypString
        | TypChar => KTypChar
        | TypBool => KTypBool
        | TypVoid => KTypVoid
        | TypExn => KTypExn
        | TypErr => KTypErr
        | TypCPointer => KTypCPointer
        | TypDecl => KTypVoid
        | TypModule => KTypModule
        | TypList(t) => KTypList(typ2ktyp_(t))
        | TypTuple(tl) => KTypTuple([for t <- tl {typ2ktyp_(t)} ])
        | TypVarTuple _ => throw compile_err(loc,
            "variable tuple type cannot be inferenced; please, use explicit type annotation")
        | TypRef(t) => KTypRef(typ2ktyp_(t))
        | TypArray(d, t) => KTypArray(d, typ2ktyp_(t))
        | TypVector(t) => KTypVector(typ2ktyp_(t))
        | TypVarArray _ => throw compile_err(loc,
            "variable array type cannot be inferenced; please, use explicit type annotation")
        | TypVarRecord => throw compile_err(loc,
            "variable record type cannot be inferenced; please, use explicit type annotation")
        | TypFun(args, rt) => KTypFun([for t <- args {typ2ktyp_(t)} ], typ2ktyp_(rt))
        | TypRecord (ref (relems, true)) =>
            KTypRecord(noid, [for (_, ni, ti, _) <- relems { (ni, typ2ktyp_(ti)) } ])
        | TypRecord _ => throw compile_err(loc,
            "the record type cannot be inferenced; use explicit type annotation")
        | TypApp(args, n) =>
            val t_opt = Ast_typecheck.find_typ_instance(t, loc)
            match t_opt {
            | Some(TypApp([], n)) =>
                match id_info(n, loc) {
                | IdVariant (ref {dvar_cases=(_, TypRecord (ref (relems, true))) :: [], dvar_flags})
                    when dvar_flags.var_flag_record =>
                    if id_stack.mem(n) {
                        throw compile_err(loc, f"the record '{n}' directly or indirectly references itself")
                    } else {
                        id_stack = n :: id_stack
                        val new_t = KTypRecord(n, [for (_, ni, ti, _) <- relems { (ni, typ2ktyp_(ti)) } ])
                        id_stack = id_stack.tl()
                        new_t
                    }
                | _ => KTypName(n)
                }
            | _ => throw compile_err(loc,
                f"the proper instance of the template type '{typ2str(t)}' is not found")
            }
        }
    }
    typ2ktyp_(t)
}

fun lit2klit(l: lit_t, ktyp: ktyp_t, loc: loc_t) =
    match l {
    | LitInt(v) => KLitInt(v)
    | LitSInt(b, v) => KLitSInt(b, v)
    | LitUInt(b, v) => KLitUInt(b, v)
    | LitFloat(b, v) => KLitFloat(b, v)
    | LitString(s) => KLitString(s)
    | LitChar(c) => KLitChar(c)
    | LitBool(f) => KLitBool(f)
    | LitEmpty =>
        match ktyp {
        | KTypList _ | KTypVector _ | KTypArray _ => KLitNil(ktyp)
        | _ => throw compile_err(loc,
            "[] is misused. It can only denote an empty collection (list, vector or array)")
        }
    | LitNull => KLitNil(KTypCPointer)
    }

var idx_access_stack: (atom_t, int) list = []

fun exp2kexp(e: exp_t, code: kcode_t, tref: bool, sc: scope_t list)
{
    val km_idx = curr_module(sc)
    val (etyp, eloc) = get_exp_ctx(e)
    //print("------------------------------------\ntranslating exp of type "); Ast_pp.pprint_typ_x(etyp, eloc); println(":"); Ast_pp.pprint_exp_x(e); println("\n==========================================\n")
    val ktyp = typ2ktyp(etyp, eloc)
    val kctx = (ktyp, eloc)
    /*
        scans through (pi, ei) pairs in for(p1<-e1, p2<-e2, ..., pn<-en) operators;
        * updates the code that needs to be put before the for loop
          (all the ei needs to be computed there),
        * generates the pattern unpack code, which should be put into the beginning of the loop body
        * generates the proxy identifiers that are used in the corresponding KExpFor/KExpMap.
        For example, the following code:
            for ((r, g, b) <- GaussianBlur(img)) { ... }
        is converted to
            val temp@@105 = GaussianBlur(img)
            for (i@@105 <- temp@@123) { val r=i@@105.0, g=i@@105.1, b = i@@105.2; ... }
    */
    fun transform_for(pe_l: (pat_t, exp_t) list, idx_pat: pat_t,
                      code: kcode_t, sc: scope_t list, body_sc: scope_t list)
    {
        val fold (idom_list, code, body_code) = ([], code, []) for (pi, ei) <- pe_l {
            val (di, code) = exp2dom(ei, code, sc)
            val ptyp =
            match di {
            | DomainRange(_, _, _) => KTypInt
            | DomainFast _ | DomainElem _ =>
                val i = match di {
                    | DomainFast(i) => i
                    | DomainElem(i) => i
                    | _ => AtomLit(KLitNil(KTypVoid))
                    }
                match get_atom_ktyp(i, eloc) {
                | KTypArray(_, et) => et
                | KTypList(et) => et
                | KTypVector(et) => et
                | KTypString => KTypChar
                | _ => throw compile_err(eloc, "unsupported type of the domain expression in for loop")
                }
            }
            val (i, body_code) = pat_simple_unpack(pi, ptyp, None, body_code, "i",
                                                default_tempval_flags(), body_sc)
            ((i, di) :: idom_list, code, body_code)
        }
        val loc = get_pat_loc(idx_pat)
        val (at_ids: id_t list, body_code: kcode_t) =
        match idx_pat {
        | PatAny _ => ([], body_code)
        | PatTyped(p, TypInt, loc) =>
            val (idx, body_code) = pat_simple_unpack(p, KTypInt, None, body_code,
                                                     "i", default_tempval_flags(), body_sc)
            (idx :: [], body_code)
        | PatTyped(p, TypTuple(tl), _) =>
            val p = pat_skip_typed(p)
            match p {
            | PatTuple(pl, _) =>
                if pl.length() != tl.length() {
                    throw compile_err(loc, "the '@' tuple pattern and its type do not match")
                }
                val fold (at_ids, body_code) = ([], body_code) for pi <- pl, ti <- tl {
                    | (_, TypInt) =>
                        val (i, body_code) = pat_simple_unpack(pi, KTypInt, None, body_code,
                                                               "i", default_tempval_flags(), body_sc)
                        (i :: at_ids, body_code)
                    | _ => throw compile_err(loc, "some of '@' indices is not an integer")
                    }
                (at_ids.rev(), body_code)
            | PatIdent(idx, _) =>
                val prefix = pp(idx)
                val fold (at_ids, ktl) = ([], []) for ti@idx <- tl {
                    | (TypInt, _) =>
                        val i = gen_idk(km_idx, f"{prefix}{idx}")
                        val _ = create_kdefval(i, KTypInt, default_tempval_flags(), None, [], loc)
                        (i :: at_ids, KTypInt :: ktl)
                    | _ => throw compile_err(loc, "some of '@' indices is not an integer")
                    }
                val ktyp = KTypTuple(ktl)
                val at_ids = at_ids.rev()
                val body_code = create_kdefval(idx, ktyp, default_tempval_flags(),
                    Some(KExpMkTuple([for i <- at_ids { AtomId(i) } ], (ktyp, loc))),
                    body_code, loc)
                (at_ids, body_code)
            | _ => throw compile_err(loc,
                "'@' pattern is expected to be either an integer scalar or a tuple of integer scalars")
            }
        | _ => throw compile_err(loc,
            "'@' pattern is expected to be either an integer scalar or a tuple of integer scalars")
        }
        (idom_list.rev(), at_ids, code, body_code)
    }

    match e {
    | ExpNop(loc) => (KExpNop(loc), code)
    | ExpBreak(_, loc) => (KExpBreak(loc), code)
    | ExpContinue(loc) => (KExpContinue(loc), code)
    | ExpReturn(e_opt, loc) =>
        val (a_opt, code) =
            match e_opt {
            | Some(e) =>
                val (a, code) = exp2atom(e, code, false, sc)
                (Some(a), code)
            | _ => (None, code)
            }
        (KExpReturn(a_opt, loc), code)
    | ExpRange(e1_opt, e2_opt, e3_opt, _) =>
        fun process_rpart(e_opt: exp_t?, code: kcode_t, defval: atom_t) =
            match e_opt {
            | Some(e) => exp2atom(e, code, false, sc)
            | _ => (defval, code)
            }
        val (a1, code) = process_rpart(e1_opt, code, _ALitVoid)
        val (a2, code) = process_rpart(e2_opt, code, _ALitVoid)
        val (a3, code) = process_rpart(e3_opt, code, AtomLit(KLitInt(1L)))
        (KExpMkTuple(a1 :: a2 :: a3 :: [], kctx), code)
    | ExpLit(LitEmpty, _) =>
        val z = gen_idk(km_idx, "z")
        val code = create_kdefval(z, ktyp, default_tempval_flags(),
            Some(KExpAtom(AtomLit(KLitNil(ktyp)), kctx)), code, eloc)
        (KExpAtom(AtomId(z), kctx), code)
    | ExpLit(lit, _) => (KExpAtom(AtomLit(lit2klit(lit, ktyp, eloc)), kctx), code)
    | ExpIdent(n, _) =>
        val new_n = match ktyp {
            | KTypVoid => noid
            | KTypName(tn) =>
                match kinfo_(tn, eloc) {
                | KVariant (ref {kvar_targs, kvar_ctors}) =>
                    try find(for nj <- kvar_ctors {get_orig_id(nj) == get_orig_id(n)})
                    catch { | NotFoundError => n }
                | _ => n
                }
            | _ => n
            }
        (if new_n == noid { KExpNop(eloc) } else { KExpAtom(AtomId(new_n), kctx) }, code)
    | ExpBinary(OpLogicAnd, e1, e2, _) =>
        val (e1, code) = exp2kexp(e1, code, false, sc)
        val eloc2 = get_exp_loc(e2)
        val (e2, code2) = exp2kexp(e2, [], false, sc)
        val e2 = rcode2kexp(e2 :: code2, eloc2)
        (KExpIf(e1, e2, KExpAtom(AtomLit(KLitBool(false)), (KTypBool, eloc2)), kctx), code)
    | ExpBinary(OpLogicOr, e1, e2, _) =>
        val (e1, code) = exp2kexp(e1, code, false, sc)
        val eloc2 = get_exp_loc(e2)
        val (e2, code2) = exp2kexp(e2, [], false, sc)
        val e2 = rcode2kexp(e2 :: code2, eloc2)
        (KExpIf(e1, KExpAtom(AtomLit(KLitBool(true)), (KTypBool, eloc2)), e2, kctx), code)
    | ExpBinary(bop, e1, e2, _) =>
        val (a1, code) = exp2atom(e1, code, false, sc)
        val (a2, code) = exp2atom(e2, code, false, sc)
        match (bop, get_atom_ktyp(a1, eloc), get_atom_ktyp(a2, eloc)) {
        | (OpAdd, KTypString, KTypString) | (OpAdd, KTypChar, KTypString) | (OpAdd, KTypString, KTypChar) =>
            (KExpIntrin(IntrinStrConcat, [a1, a2 ], kctx), code)
        | _ => (KExpBinary(bop, a1, a2, kctx), code)
        }
    | ExpUnary(OpDeref, e, _) =>
        val (a_id, code) = exp2id(e, code, false, sc, "a literal cannot be dereferenced")
        (KExpUnary(OpDeref, AtomId(a_id), kctx), code)
    | ExpUnary(OpDotMinus, e, _) =>
        val (arr, idx_i) =
        match idx_access_stack {
        | (arr, idx_i) :: _ => (arr, idx_i)
        | _ => throw compile_err(eloc, ".- is only allowed inside array access op. .- inside of tuple indexes is not supported too.")
        }
        val (a, code) = exp2atom(e, code, false, sc)
        val args = if idx_i == 0 { [arr] }
                   else { [arr, AtomLit(KLitInt(int64(idx_i))) ] }
        val (sz, code) = kexp2atom(km_idx, "sz", KExpIntrin(IntrinGetSize,
                                    args, (KTypInt, eloc)), false, code)
        (KExpBinary(OpSub, sz, a, kctx), code)
    | ExpUnary(uop, e1, _) =>
        val (a1, code) = exp2atom(e1, code, false, sc)
        (KExpUnary(uop, a1, kctx), code)
    | ExpIntrin(iop, args, _) =>
        val fold (args, code) = ([], code) for ei <- args {
            val (ai, code) = exp2atom(ei, code, false, sc)
            (ai :: args, code)
        }
        (KExpIntrin(iop, args.rev(), kctx), code)
    | ExpSeq(eseq, _) =>
        val sc = new_block_scope(km_idx) :: sc
        val code = transform_all_types_and_cons(eseq, code, sc)
        val (code, _) = eseq2code(eseq, code, sc)
        match code {
        | c :: code => (c, code)
        | _ => (KExpNop(eloc), code)
        }
    | ExpSync(n, e0) =>
        val (e, code1) = exp2kexp(e0, [], false, sc)
        (KExpSync(n, rcode2kexp(e :: code1, get_exp_loc(e0))), code)
    | ExpMkTuple(args, _) =>
        val fold (args, code) = ([], code) for ei <- args {
            val (ai, code) = exp2atom(ei, code, false, sc)
            (ai :: args, code)
        }
        (KExpMkTuple(args.rev(), kctx), code)
    | ExpMkArray(arows, _) =>
        if arows == [] {
            throw compile_err(eloc, "empty arrays are not supported")
        }
        val fold krows=[], code=code, all_literals=true for arow <- arows {
            val fold krow=[], code=code, all_literals=all_literals for e <- arow {
                val (f, e, islit) = match e {
                             | ExpUnary(OpExpand, e, _) => (true, e, false)
                             | ExpLit((LitString _ | LitNull | LitEmpty), _) => (false, e, false)
                             | ExpLit(_, _) => (false, e, true)
                             | _ => (false, e, false)
                             }
                val (a, code) = exp2atom(e, code, false, sc)
                ((f, a) :: krow, code, all_literals & islit)
            }
            (krow.rev() :: krows, code, all_literals)
        }
        (KExpMkArray(all_literals, krows.rev(), kctx), code)
    | ExpMkVector(elems, _) =>
        if elems == [] {
            throw compile_err(eloc, "empty vector literals are not supported")
        }
        val fold elems=[], code=code for e <- elems {
            val (f, e) = match e {
                | ExpUnary(OpExpand, e, _) => (true, e)
                | _ => (false, e)
                }
            val (a, code) = exp2atom(e, code, false, sc)
            ((f, a) :: elems, code)
        }
        (KExpMkVector(elems.rev(), kctx), code)
    | ExpMkRecord(rn, rinitelems, _) =>
        val (rn_id, ctor, relems) =
            match (rn, deref_typ(etyp)) {
            | (ExpIdent(rn_id, _), _) =>
                val (ctor, relems) = Ast_typecheck.get_record_elems(Some(rn_id), etyp, false, eloc)
                (rn_id, ctor, relems)
            | (ExpNop _, TypRecord (ref (relems, true))) => (noid, noid, relems)
            | _ => throw compile_err(get_exp_loc(rn),
                    "k-normalization: in the record construction identifier is expected after type check")
            }
        val fold (ratoms, code) = ([], code) for (_, ni, ti, vi) <- relems {
            val (a, code) =
            match find_opt(for (nj, _) <- rinitelems { ni == nj }) {
            | Some((_, ej)) => exp2atom(ej, code, false, sc)
            | _ =>
                match vi {
                | ExpNop _ =>
                    throw compile_err(eloc, f"there is no explicit initializer for \
                        the field '{pp(rn_id)}.{pp(ni)}' nor there is default initializer for it")
                | _ =>
                    exp2atom(vi, code, false, sc)
                }
            }
            (a :: ratoms, code)
        }
        if ctor == noid { (KExpMkRecord(ratoms.rev(), kctx), code) }
        else { (KExpCall(ctor, ratoms.rev(), kctx), code) }
    | ExpUpdateRecord(e, new_elems, _) =>
        val (rec_n, code) = exp2id(e, code, true, sc, "the updated record cannot be a literal")
        val (_, relems) = Ast_typecheck.get_record_elems(None, etyp, false, eloc)
        val fold (ratoms, code) = ([], code) for (_, ni, ti, _)@idx <- relems {
            val (a, code) =
                try {
                    val (_, ej) = find(for (nj, ej) <- new_elems { ni == nj })
                    exp2atom(ej, code, false, sc)
                } catch {
                | NotFoundError =>
                    val ni_ = dup_idk(km_idx, ni)
                    val ti_ = typ2ktyp(ti, eloc)
                    val get_ni = KExpMem(rec_n, idx, (ti_, eloc))
                    val code = create_kdefval(ni_, ti_, default_tempref_flags(), Some(get_ni), code, eloc)
                    (AtomId(ni_), code)
                }
            (a :: ratoms, code)
        }
        (KExpMkRecord(ratoms.rev(), kctx), code)
    | ExpCall(f, args, _) =>
        val (args, kwarg_opt) =
            match args.rev() {
            | (ExpMkRecord(ExpNop _, _, _) as mkrec) :: rest => (rest.rev(), Some(mkrec))
            | _ => (args, None)
            }
        val fold (args, code) = ([], code) for ei <- args {
            val (ai, code) = exp2atom(ei, code, false, sc)
            (ai :: args, code)
        }
        val (args, code) =
            match kwarg_opt {
            | Some(e) =>
                val (ke, code) = exp2kexp(e, code, false, sc)
                match ke {
                | KExpMkRecord(rest_args, _) => (args.rev() + rest_args, code)
                | _ => throw compile_err(get_exp_loc(e), "the expression should convert to KExpMkRecord()")
                }
            | _ => (args.rev(), code)
            }
        val (f_exp, code) = exp2kexp(f, code, false, sc)
        match f_exp {
        | KExpMem(obj, idx, (_, f_loc)) =>
            val t = get_idk_ktyp(obj, f_loc)
            match get_kinterface_opt(t, f_loc) {
            | Some(iface) =>
                (KExpICall(obj, idx, args, kctx), code)
            | _ =>
                val (f_id, code) = kexp2id(km_idx, "f", f_exp, true, code,
                                    "cannot reduce obj.idx to an id (?!)")
                (KExpCall(f_id, args, kctx), code)
            }
        | _ =>
            val (f_id, code) = kexp2id(km_idx, "f", f_exp, true, code,
                                "cannot reduce function expression to an id")
            (KExpCall(f_id, args, kctx), code)
        }
    | ExpThrow(e, _) =>
        val (a_id, code) = exp2id(e, code, false, sc, "a literal cannot be thrown as exception")
        (KExpThrow(a_id, false, eloc), code)
    | ExpIf(e1, e2, e3, _) =>
        val (c, code) = exp2atom(e1, code, false, sc)
        val loc1 = get_exp_loc(e1)
        val loc2 = get_exp_loc(e2)
        val loc3 = get_exp_loc(e3)
        val (e2, code2) = exp2kexp(e2, [], false, sc)
        val (e3, code3) = exp2kexp(e3, [], false, sc)
        val if_then = rcode2kexp(e2 :: code2, loc2)
        val if_else = rcode2kexp(e3 :: code3, loc3)
        (KExpIf(KExpAtom(c, (KTypBool, loc1)), if_then, if_else, kctx), code)
    | ExpWhile(e1, e2, _) =>
        val loc1 = get_exp_loc(e1)
        val loc2 = get_exp_loc(e2)
        val (e1, code1) = exp2kexp(e1, [], false, sc)
        val (e2, code2) = exp2kexp(e2, [], false, sc)
        val c = rcode2kexp(e1 :: code1, loc1)
        val body = rcode2kexp(e2 :: code2, loc2)
        (KExpWhile(c, body, eloc), code)
    | ExpDoWhile(e1, e2, _) =>
        val (e1, code1) = exp2kexp(e1, [], false, sc)
        val (e2, code2) = exp2kexp(e2, e1 :: code1, false, sc)
        val body = rcode2kexp(code2, eloc)
        (KExpDoWhile(body, e2, eloc), code)
    | ExpFor(pe_l, idx_pat, body, flags, _) =>
        val body_sc = new_block_scope(km_idx) :: sc
        val (idom_list, at_ids, code, body_code) = transform_for(pe_l, idx_pat, code, sc, body_sc)
        val (last_e, body_code) = exp2kexp(body, body_code, false, body_sc)
        val bloc = get_exp_loc(body)
        val body_kexp = rcode2kexp(last_e :: body_code, bloc)
        (KExpFor(idom_list, at_ids, body_kexp, flags, eloc), code)
    | ExpMap(pew_ll, body, flags, _) =>
        /*
            process the nested for clauses. since there can be non-trivial patterns
            and non-trivial iteration domain expressions, transform_for will produce
            some "pre_code", i.e. the code that needs to be executed before (outside of)
            each clause of the nested loop and also the "body_code" that needs to be
            computed inside the loop (i.e. the pattern unpacking) in the beginning
            before all other expressions. In the case of nested loop in exp-map this
            body_code will actually become the outer code for the nested loop.
            So, it's passed to the next iteration of 'fold' and will prepend
            the next "pre_code". Finally, the body_code from the last iteration, i.e.
            from the most inner for loop will actually become the prefix of the actual
            body code that is transformed after this 'fold'.
        */
        val body_sc = new_block_scope(km_idx) :: sc
        val (pre_idom_ll, body_code) =
        fold (pre_idom_ll, prev_body_code) = ([], []) for (pe_l, idx_pat) <- pew_ll {
            val (idom_list, at_ids, pre_code, body_code) =
                transform_for(pe_l, idx_pat, prev_body_code, sc, body_sc)
            val pre_exp = rcode2kexp(pre_code, eloc)
            ((pre_exp, idom_list, at_ids) :: pre_idom_ll, body_code)
        }
        val (last_e, body_code) = exp2kexp(body, body_code, false, body_sc)
        val bloc = get_exp_loc(body)
        val body_kexp = rcode2kexp(last_e :: body_code, bloc)
        (KExpMap(pre_idom_ll.rev(), body_kexp, flags, kctx), code)
    | ExpAt(e, border, interp, idxlist, _) =>
        val (arr, code) = exp2atom(e, code, true, sc)
        val probably_tuple_type = match idxlist{
            | idx::[] => deref_typ(get_exp_typ(idx))
            | _ => TypVoid
        }

        fun is_exprange(e:exp_t) = match e {
            | ExpRange _ => true
            | _ => false
        }

        fun cast_if_needed(scalar_idx: atom_t, code: kcode_t, loc:loc_t): (atom_t, kcode_t) = 
            match get_atom_ktyp(scalar_idx, loc) {
            | KTypBool | KTypUInt(_) | KTypSInt(_) => 
                kexp2atom(curr_module(sc), "idx", KExpCast(scalar_idx, KTypInt, loc), false, code)
            | _ => (scalar_idx, code)
        }

        val (dlist, code) = match (idxlist, probably_tuple_type) {
        | (tupidx::tl, TypTuple(idxtype)) when !is_exprange(tupidx) =>
            val (_, iloc) = get_exp_ctx(tupidx) 
            if (tl.length() != 0) {
                throw compile_err(eloc, "internal error: tuple index is not only")
            }
            val (tup_id, code) = exp2id(tupidx, code, false, sc, "internal error: a literal instead of tuple")
            fold (dlist, code) = ([], code) for eltyp@elnum <- idxtype{
                val (d, code) = kexp2atom(curr_module(sc), "idx", KExpMem(tup_id, elnum, (typ2ktyp(eltyp, iloc), iloc)), false, code)
                val (d, code) = cast_if_needed(d, code, iloc)
                (DomainElem(d)::dlist, code)
            }
        | _ => 
            fold (dlist, code) = ([], code) for i@idx <- idxlist {
                idx_access_stack = (arr, idx) :: idx_access_stack
                val (d, code) =
                    try exp2dom(i, code, sc)
                    finally { idx_access_stack = idx_access_stack.tl() }
                val (_, iloc) = get_exp_ctx(i)
                val (d, code) = match d {
                    |DomainElem(scalar_idx) => 
                        val (scalar_idx, code) = cast_if_needed(scalar_idx, code, iloc)
                        (DomainElem(scalar_idx), code)
                    | _ => (d, code)
                }
                (d :: dlist, code)
            }
        }
        (KExpAt(arr, border, interp, dlist.rev(), kctx), code)
    | ExpMem(e1, elem, _) =>
        val e1loc = get_exp_loc(e1)
        val (a_id, code) = exp2id(e1, code, true, sc, "the literal does not have members to access")
        val ktyp = get_idk_ktyp(a_id, e1loc)
        fun find_relem(rn, relems, elem_id, loc) {
            val (i, _) = fold (i, j) = (-1, 0) for (ni, _) <- relems {
                if elem_id == ni { (j, j + 1) } else { (i, j + 1) }
            }
            if i >= 0 { i } else {
                throw compile_err(loc, f"there is no record field '{elem_id}' in the record '{pp(rn)}'")
            }
        }

        match (ktyp, elem) {
        | (KTypTuple(tl), ExpLit(LitInt(idx_), (ityp, iloc))) =>
            val idx = int(idx_)
            val n = tl.length()
            if !(0 <= idx < n) {
                throw compile_err(iloc, f"the tuple index is outside of the range [0, {n})")
            }
            (KExpMem(a_id, idx, kctx), code)
        | (KTypRecord(rn, relems), ExpIdent(n, (_, nloc))) =>
            val i = find_relem(rn, relems, n, eloc)
            (KExpMem(a_id, i, kctx), code)
        | (KTypName(tn), ExpIdent(n, (_, nloc)))
            when (match kinfo_(tn, eloc) { | KInterface _ => true | _ => false }) =>
            val iface = get_iface(tn, eloc)
            var idx = -1
            for (f, _, _)@i <- iface->di_all_methods {
                if f == n { idx = i; break }
            }
            if idx < 0 {
                throw compile_err(eloc,
                f"k-norm: method '{pp(n)}' is not found in interface '{idk2str(tn, eloc)}'")
            }
            (KExpMem(a_id, idx, kctx), code)
        | (KTypName(tn), ExpIdent(n, (_, nloc))) =>
            if n == std__tag__ {
                (KExpIntrin(IntrinVariantTag, AtomId(a_id) :: [], (KTypCInt, eloc)), code)
            } else {
                val ((ctor, case_i, vt, _), relems) = get_record_elems_k(None, ktyp, eloc)
                val get_vcase = KExpIntrin(IntrinVariantCase, [AtomId(a_id), AtomLit(KLitInt(int64(case_i))) ], (vt, eloc))
                val i = find_relem(ctor, relems, n, eloc)
                val (get_elem, code) =
                if relems.length() == 1 {
                    (get_vcase, code)
                } else {
                    val (v_id, code) = kexp2id(km_idx, "vcase", get_vcase, true, code,
                                "variant case extraction should produce id, not literal")
                    (KExpMem(v_id, i, kctx), code)
                }
                (get_elem, code)
            }
        | (KTypExn, ExpIdent(n, (etyp2, eloc2)))
            when n == std__tag__ =>
            (KExpIntrin(IntrinVariantTag, AtomId(a_id) :: [], (KTypCInt, eloc)), code)
        | (_, ExpIdent(n, (etyp2, eloc2))) =>
            throw compile_err(e1loc,
                f"unsupported '(some_struct : {typ2str(get_exp_typ(e1))}).{pp(n)}' access operation")
        | (_, _) => throw compile_err(e1loc, "unsupported access operation")
        }
    | ExpAssign(e1, e2, _) =>
        val (a2, code) = exp2atom(e2, code, false, sc)
        val (a_id, code) = exp2id(e1, code, true, sc, "a literal cannot be assigned")
        val kv = get_kval(a_id, eloc)
        val {kv_flags, kv_typ} = kv
        val kv_flags = match (e1, kv_typ) {
                       | (ExpAt(_, _, _, _, _), KTypArray(_, _)) => kv_flags.{val_flag_subarray=true}
                       | _ => kv_flags
                       }
        val kv = kv.{kv_flags=kv_flags.{val_flag_mutable=true}}
        set_idk_entry(a_id, KVal(kv))
        (KExpAssign(a_id, a2, eloc), code)
    | ExpCast(e, _, _) =>
        val (a, code) = exp2atom(e, code, false, sc)
        (KExpCast(a, ktyp, eloc), code)
    | ExpTyped(e, t, _) =>
        val (a, code) = exp2atom(e, code, false, sc)
        val t = typ2ktyp(t, eloc)
        (KExpAtom(a, (t, eloc)), code)
    | ExpCCode(s, _) => (KExpCCode(s, kctx), code)
    | ExpData(kind, fname, _) =>
        (embed_data(kind, fname, kctx), code)
    | ExpMatch(e1, cases, _) =>
        val loc1 = get_exp_loc(e1)
        val (a, code) = exp2atom(e1, code, false, sc)
        val (b, code) =
        if !is_mutable_atom(a, loc1) {
            (a, code)
        } else {
            val a_id = match a {
                       | AtomId(a_id) => a_id
                       | _ => throw compile_err(loc1, "k-norm: invalid mutable atom (id is expected)")
                       }
            val t = get_atom_ktyp(a, loc1)
            val b = dup_idk(km_idx, a_id)
            val code = create_kdefval(b, t, default_tempval_flags(),
                        Some(KExpAtom(a, (t, loc1))), code, loc1)
            (AtomId(b), code)
        }
        val (k_cases, code) = transform_pat_matching(b, cases, code, sc, eloc, false)
        (KExpMatch(k_cases, kctx), code)
    | ExpTryCatch(e1, cases, _) =>
        val e1loc = get_exp_loc(e1)
        val try_sc = new_block_scope(km_idx) :: sc
        val (e1, body_code) = exp2kexp(e1, [], false, try_sc)
        val try_body = rcode2kexp(e1 :: body_code, e1loc)
        val exn_loc = match cases {
                      | (p, _) :: _ => get_pat_loc(p)
                      | _ => eloc
                      }
        val exn_n = gen_idk(km_idx, "exn")
        val pop_e = KExpIntrin(IntrinPopExn, [], (KTypExn, exn_loc))
        val catch_sc = new_block_scope(km_idx) :: sc
        val catch_code = create_kdefval(exn_n, KTypExn, default_val_flags(), Some(pop_e), [], exn_loc)
        val (k_cases, catch_code) = transform_pat_matching(AtomId(exn_n),
                        cases, catch_code, catch_sc, exn_loc, true)
        val handle_exn = match k_cases {
            | ([], e) :: [] => e
            | _ => KExpMatch(k_cases, (ktyp, exn_loc))
            }
        val handle_exn = rcode2kexp(handle_exn :: catch_code, exn_loc)
        (KExpTryCatch(try_body, handle_exn, kctx), code)
    | DefVal(p, e2, flags, _) =>
        val (e2, code) = exp2kexp(e2, code, true, sc)
        val ktyp = get_kexp_typ(e2)
        match (p, ktyp) {
        | (PatIdent(n, _), KTypVoid) =>
            val dv = kdefval_t {kv_name=n, kv_cname="", kv_typ=ktyp, kv_flags=flags, kv_loc=eloc}
            set_idk_entry(n, KVal(dv))
            (e2, code)
        | _ =>
            /*  if pat_simple_unpack returns (noid, code), it means that the pattern p does
                not contain variables to capture, i.e. user wrote something like
                    val _ = <exp> or
                    val (_, (_, _)) = <exp> etc.,
                which means that the assignment was not generated, but we need to retain <exp>,
                because it likely has some side effects */
            val (v, code) = pat_simple_unpack(p, ktyp, Some(e2), code, "v", flags, sc)
            if v == noid { (e2, code) } else { (KExpNop(eloc), code) }
        }
    | DefFun(df) => val code = transform_fun(df, code, sc)
                    (KExpNop(eloc), code)
    | DefTyp _ => (KExpNop(eloc), code)
    // variant declarations are handled in batch in transform_all_types_and_cons
    | DefVariant _ => (KExpNop(eloc), code)
    // exception declarations are handled in batch in transform_all_types_and_cons
    | DefExn _ => (KExpNop(eloc), code)
    // interface declarations are handled in batch in transform_all_types_and_cons
    | DefInterface _ => (KExpNop(eloc), code)
    | DirImport(_, _) => (KExpNop(eloc), code)
    | DirImportFrom(_, _, _) => (KExpNop(eloc), code)
    | DirPragma(_, _) => (KExpNop(eloc), code)
    }
}

fun exp2atom(e: exp_t, code: kcode_t, tref: bool, sc: scope_t list): (atom_t, kcode_t)
{
    val (e, code) = exp2kexp(e, code, tref, sc)
    kexp2atom(curr_module(sc), "v", e, tref, code)
}

fun exp2id(e: exp_t, code: kcode_t, tref: bool, sc: scope_t list, msg: string): (id_t, kcode_t)
{
    val (a, code) = exp2atom(e, code, tref, sc)
    (atom2id(a, get_exp_loc(e), msg), code)
}

fun exp2dom(e: exp_t, code: kcode_t, sc: scope_t list): (dom_t, kcode_t) =
    match e {
    | ExpRange _ =>
        val (ek, code) = exp2kexp(e, code, false, sc)
        match ek {
        | KExpMkTuple(a :: b :: c :: [], _) => (DomainRange(a, b, c), code)
        | _ => throw compile_err(get_exp_loc(e),
            "the range was not converted to a 3-element tuple as expected")
        }
    | _ =>
        val (i, code) = exp2atom(e, code, false, sc)
        (DomainElem(i), code)
    }

type kpragma_t = (string, loc_t)
fun eseq2code(eseq: exp_t list, code: kcode_t, sc: scope_t list): (kcode_t, kpragma_t list)
{
    var pragmas: kpragma_t list = []
    fun knorm_eseq(eseq: exp_t list, code: kcode_t): kcode_t =
        match eseq {
        | DirPragma(prl, loc) :: rest =>
            for pr <- prl { pragmas = (pr, loc) :: pragmas }
            knorm_eseq(rest, code)
        | ei :: rest =>
            val (eki, code) = exp2kexp(ei, code, false, sc)
            val code = match eki {
                       | KExpNop _ => code
                       | _ => eki :: code
                       }
            knorm_eseq(rest, code)
        | [] => code
        }

    val code = knorm_eseq(eseq, code)
    (code, pragmas)
}

/* finds if the pattern contains variables to capture. We could have
   combined this and the next function into one, but then we would
   have to scan the whole pattern (or need more complex code
   to do early exit).
   Besides, most of the time (for value declarations, loop iteration variables,
   function arguments ...) we know already that a pattern does not need checks,
   so we just need have_variables for it */
fun pat_have_vars(p: pat_t): bool
{
    | PatAny _ | PatLit(_, _) => false
    | PatIdent(_, _) | PatAs(_, _, _) => true
    | PatCons(p1, p2, _) => pat_have_vars(p1) || pat_have_vars(p2)
    | PatTyped(p, _, _) => pat_have_vars(p)
    | PatTuple(pl, _) => pl.exists(pat_have_vars)
    | PatVariant(_, pl, _) => pl.exists(pat_have_vars)
    | PatRecord(_, ip_l, _) => ip_l.exists(fun ((_, pi)) {pat_have_vars(pi)})
    | PatRef(p, _) => pat_have_vars(p)
    | PatWhen(p, _, _) => pat_have_vars(p)
    | PatAlt(pl, _) => pl.exists(pat_have_vars)
}

/* version of Ast_typecheck.get_record_elems, but for already transformed types */
fun get_record_elems_k(vn_opt: id_t?, t: ktyp_t, loc: loc_t): ((id_t, int, ktyp_t, bool), (id_t, ktyp_t) list)
{
    val t = deref_ktyp(t, loc)
    val input_vn = match vn_opt { | Some(vn) => get_bare_name(vn) | _ => noid }
    match t {
    | KTypRecord(_, relems) => ((noid, 0, t, false), relems)
    | KTypName(tn) =>
        match kinfo_(tn, loc) {
        | KVariant (ref {kvar_flags, kvar_cases=(vn0, KTypRecord(_, relems) as rectyp) :: []})
            when kvar_flags.var_flag_record =>
            if input_vn != noid && input_vn != get_orig_id(vn0) {
                throw compile_err(loc,
                    f"mismatch in the record name: given '{pp(input_vn)}', expected '{pp(vn0)}'")
            }
            ((noid, 1, rectyp, false), relems)
        | KVariant (ref {kvar_cases, kvar_ctors}) =>
            val single_case = match kvar_cases { | _ :: [] => true | _ => false }
            match find_opt(for (vn, t)@i <- kvar_cases, c_id <- kvar_ctors {
                        get_orig_id(vn) == input_vn || (single_case && input_vn == noid) }) {
            | Some(((_, KTypRecord(_, relems)), i, ctor)) =>
                val rectyp = match relems {
                             | (_, t) :: [] => t
                             | _ => KTypTuple([for (_, t) <- relems {t} ])
                             }
                ((ctor, i+1, rectyp, kvar_cases.length() > 1), relems)
            | _ => throw compile_err(loc, f"tag '{pp(input_vn)}' is not found or is not a record")
            }
        | _ => throw compile_err(loc, f"type '{tn}' is expected to be a variant")
        }
    | _ => throw compile_err(loc, "k-norm: attempt to treat non-record and non-variant as a record")
    }
}

fun match_record_pat(pat: pat_t, ptyp: ktyp_t):
    ((id_t, int, ktyp_t, bool, bool), (id_t, pat_t, ktyp_t, int) list) =
    match pat {
    | PatRecord(rn_opt, relems, loc) =>
        val ((ctor, case_i, t, multiple_cases), relems_found) = get_record_elems_k(rn_opt, ptyp, loc)
        val fold typed_rec_pl = [] for (ni, pi) <- relems {
            val ni_orig = get_orig_id(ni)
            val fold (found_idx, found_t) = (-1, KTypVoid) for (nj, tj)@idx <- relems_found {
                if get_orig_id(nj) == ni_orig { (idx, tj) }
                else { (found_idx, found_t) }
            }
            if found_idx < 0 {
                throw compile_err(loc,
                    f"element '{pp(ni)}' is not found in the record '{pp(rn_opt.value_or(noid))}'")
            }
            (ni, pi, found_t, found_idx) :: typed_rec_pl
        }
        ((ctor, case_i, t, multiple_cases, relems_found.length() > 1), typed_rec_pl)
    | _ => throw compile_err(get_pat_loc(pat), "record (or sometimes an exception) is expected")
    }

fun match_variant_pat(pat: pat_t, ptyp: ktyp_t): ((id_t, int, ktyp_t), (pat_t, ktyp_t) list) =
    match pat {
    | PatVariant(vn0, pl, loc) =>
        val {kvar_cases, kvar_ctors, kvar_loc} = *get_kvariant(ptyp, loc)
        match find_opt(for (vn, t)@i <- kvar_cases, c_id <- kvar_ctors {
                        get_orig_id(vn) == get_orig_id(vn0) }) {
        | Some(((_, t), idx, ctor)) =>
            val tl = match t { | KTypTuple(tl) => tl | _ => t :: [] }
            if pl.length() != tl.length() {
                throw compile_err( loc,
                    f"the number of variant pattern arguments does not match \
                    the number of '{pp(ctor)}' parameters.\nSee {kvar_loc}")
            }
            val typed_var_pl = [for pi <- pl, ti <- tl { (pi, ti) } ]
            ((ctor, idx+1, t), typed_var_pl)
        | _ => throw compile_err(loc, f"tag '{pp(vn0)}' is not found or is not a record")
        }
    | _ => throw compile_err(get_pat_loc(pat), "variant pattern is expected")
    }

fun pat_need_checks(p: pat_t, ptyp: ktyp_t)
{
    /* in the case of exceptions we always need to check the tag,
        so the response from pat_need_checks() is 'true' */
    fun check_if_exn(e: exn, loc: loc_t) =
        match deref_ktyp(ptyp, loc) {
        | KTypExn => true
        | _ => throw e
        }
    match p {
    | PatAny _ | PatIdent(_, _) | PatAs(_, _, _) => false
    | PatLit(_, _) => true
    | PatCons(_, _, _) => true
    | PatTyped(p, _, _) => pat_need_checks(p, ptyp)
    | PatTuple(pl, loc) =>
        val tl = match ptyp {
                 | KTypTuple(tl) => tl
                 | _ => throw compile_err(loc, "this pattern needs a tuple as argument")
                 }
        exists(for pi <- pl, ti <- tl {pat_need_checks(pi, ti)})
    | PatVariant(vn, pl, loc) =>
        try {
            val {kvar_cases} = *get_kvariant(ptyp, loc)
            kvar_cases.length() > 1 ||
            ({
                val (_, typed_var_pl) = match_variant_pat(p, ptyp)
                exists(for (p, t) <- typed_var_pl {pat_need_checks(p, t)})
            })
        } catch { | CompileError(_, _) as e => check_if_exn(e, loc) }
    | PatRecord(rn_opt, _, loc) =>
        try {
            val ((_, _, _, multiple_cases, _), typed_rec_pl) = match_record_pat(p, ptyp)
            multiple_cases || exists(for (_, pi, ti, _) <- typed_rec_pl {pat_need_checks(pi, ti)})
        } catch { | CompileError(_, _) as e => check_if_exn(e, loc) }
    | PatRef(p, loc) =>
        val t = match ptyp {
                | KTypRef(t) => t
                | _ => throw compile_err(loc, "this pattern needs a reference as argument")
                }
        pat_need_checks(p, t)
    | PatWhen(_, _, _) => true
    | PatAlt(pl, _) =>
        exists(for pi <- pl {pat_need_checks(pi, ptyp)})
    }
}

fun pat_propose_id(p: pat_t, ptyp: ktyp_t, temp_prefix: string,
                   is_simple: bool, mutable_leaves: bool, sc: scope_t list)
{
    val km_idx = curr_module(sc)
    val p = pat_skip_typed(p)
    match p {
    | PatAny _ => (p, noid, false, true)
    | PatIdent(n, _) => (p, n, false, false)
    | PatAs(p, n, ploc) =>
        if mutable_leaves {
            throw compile_err(ploc, "'as' pattern cannot be used with var's, only with values")
        }
        (pat_skip_typed(p), n, false, false)
    | PatRef(_, _) =>
        if pat_have_vars(p) || !is_simple && pat_need_checks(p, ptyp) {
            (p, gen_idk(km_idx, temp_prefix), false, true)
        } else {
            (p, noid, false, true)
        }
    | _ =>
        if pat_have_vars(p) || !is_simple && pat_need_checks(p, ptyp) {
            (p, gen_idk(km_idx, temp_prefix), true, true)
        } else {
            (p, noid, false, true)
        }
    }
}

fun pat_simple_unpack(p: pat_t, ptyp: ktyp_t, e_opt: kexp_t?, code: kcode_t,
                      temp_prefix: string, flags: val_flags_t, sc: scope_t list): (id_t, kcode_t)
{
    val km_idx = curr_module(sc)
    val (tup_elems, need_tref) =
        match e_opt {
        | Some(e) =>
            match e {
            | KExpIntrin(_, _, _) | KExpAt(_, _, _, _, _)
            | KExpMem(_, _, _) | KExpUnary(OpDeref, _, _) => ([], true)
            | KExpMkTuple(elems, _) => (elems, false)
            | _ => ([], false)
            }
        | None => ([], true)
        }
    val mutable_leaves = flags.val_flag_mutable
    val n_flags = flags.{val_flag_mutable=false, val_flag_tempref=false}
    val (p, n, tref, tmp) = pat_propose_id(p, ptyp, temp_prefix, true, mutable_leaves, sc)
    val tref = tref && need_tref
    if n == noid {
        (n, code)
    } else {
        val loc = get_pat_loc(p)
        val n_flags =
            if mutable_leaves && !tref { n_flags.{val_flag_mutable=true} }
            else if tref { n_flags.{val_flag_tempref=true} }
            else if tmp { n_flags.{val_flag_temp=true} }
            else { n_flags }
        val n_flags = match sc {
                      | ScModule _ :: _ => n_flags.{val_flag_global=sc}
                      | _ => n_flags
                      }
        val code = create_kdefval(n, ptyp, n_flags, e_opt, code, loc)
        val code = match p {
            | PatTuple(pl, loc) =>
                val tl =
                match ptyp {
                | KTypTuple(tl) =>
                    if tl.length() != pl.length() {
                        throw compile_err(loc,
                            "the number of elements in the pattern and in the tuple type are different")
                    } else {
                        tl
                    }
                | _ => throw compile_err(loc,
                    "invalid type of the tuple pattern (it must be a tuple as well)")
                }
                fold code=code for pi@idx <- pl, ti <- tl {
                    val loci = get_pat_loc(pi)
                    val ei = if tup_elems != [] { KExpAtom(tup_elems.nth(idx), (ti, loc)) }
                             else { KExpMem(n, idx, (ti, loci)) }
                    pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
                }
            | PatIdent(_, _) => code
            | PatVariant(vn, _, loc) =>
                val ((_, case_i, vt), typed_var_pl) = match_variant_pat(p, ptyp)
                val get_vcase = KExpIntrin(IntrinVariantCase, [AtomId(n), AtomLit(KLitInt(int64(case_i))) ], (vt, loc))
                match typed_var_pl {
                | (p, t) :: [] =>
                    pat_simple_unpack(p, t, Some(get_vcase), code, temp_prefix, flags, sc).1
                | _ =>
                    val (ve, code) = kexp2atom(km_idx, "vcase", get_vcase, true, code)
                    val ve_id = atom2id(ve, loc, "variant case extraction should produce id, not literal")
                    fold code = code for (pi, ti)@idx <- typed_var_pl {
                        val loci = get_pat_loc(pi)
                        val ei = KExpMem(ve_id, idx, (ti, loci))
                        pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
                    }
                }
            | PatRecord(rn_opt, _, _) =>
                val ((ctor, case_i, rectyp, _, multiple_relems), typed_rec_pl) = match_record_pat(p, ptyp)
                val (r_id, get_vcase, code2) =
                if ctor == noid {
                    (n, KExpNop(loc), code)
                } else {
                    /*val case_i = match rn_opt {
                                | Some(rn) => rn
                                | _ => throw compile_err(loc, "record tag should be non-empty here")
                                }*/
                    val get_vcase = KExpIntrin(IntrinVariantCase, [AtomId(n), AtomLit(KLitInt(int64(case_i))) ], (rectyp, loc))
                    val (r, code2) = kexp2atom(km_idx, "vcase", get_vcase, true, code)
                    (atom2id(r, loc, "variant case extraction should produce id, not literal"), get_vcase, code2)
                }
                match (ctor != noid, multiple_relems, typed_rec_pl) {
                | (true, false, (_, p, t, _) :: []) =>
                    pat_simple_unpack(p, t, Some(get_vcase), code, temp_prefix, flags, sc).1
                | _ =>
                    fold code = code2 for (_, pi, ti, ii) <- typed_rec_pl {
                        val ei = KExpMem(r_id, ii, (ti, loc))
                        pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
                    }
                }
            | PatAs(_, _, _) =>
                val e = KExpAtom(AtomId(n), (ptyp, loc))
                pat_simple_unpack(p, ptyp, Some(e), code, temp_prefix, flags, sc).1
            | PatRef(p, loc) =>
                val t = match ptyp {
                        | KTypRef(t) => t
                        | _ => throw compile_err(loc, "the argument of ref() pattern must be a reference")
                        }
                val e = KExpUnary(OpDeref, AtomId(n), (t, loc))
                pat_simple_unpack(p, t, Some(e), code, temp_prefix, n_flags, sc).1
            | _ => throw compile_err(loc, "this type of pattern cannot be used here")
            }
        (n, code)
    }
}

type pat_info_t = {pinfo_p: pat_t; pinfo_typ: ktyp_t; pinfo_e: kexp_t; pinfo_tag: id_t}

/*
    Transforms pattern matching constructions into a sequence of nested if's,
    followed by the actions.

    We dynamically maintain 3 lists of the sub-patterns to consider next.
    Each new sub-pattern occuring during recursive processing of the top-level pattern
    is classified and then is either discarded or added into one of the 3 lists:
    * pl_c - the patterns that needs some checks to verify, but have no captured variables
    * pl_uc - need checks and have variables to capture
    * pl_u - need no checks, but have variables to capture.
    The first list pl_c grows from the both ends:
        * literals, as the easiest to check patterns, are added to the beginning of the list.
            So they get a higher priority.
        * other patterns are added to the end

    When we need to select the next sub-pattern to process, we first look at the first list (pl_c),
    if it's empty then we look at the second list (pl_uc) and finally we look at the third list (pl_u).
    Some sub-patterns in pl_uc could be then added to pl_c or pl_u (or discarded).

    We do such dispatching in order to minimize the number of read operations from a complex structure.
    That is, why capture a variable until all the checks are complete and we know we have a match.
    The algorithm does not always produce the most optimal sequence of operations
    (e.g. some checks are easier to do than the others etc.), but probably it's a good-enough approximation
*/
fun transform_pat_matching(a: atom_t, cases: (pat_t, exp_t) list,
                           code: kcode_t, sc: scope_t list, loc: loc_t, catch_mode: bool)
{
    var match_var_cases = empty_idset
    val km_idx = curr_module(sc)

    fun dispatch_pat(pinfo: pat_info_t,
        (pl_c: pat_info_t list, pl_cu: pat_info_t list, pl_u: pat_info_t list))
    {
        val {pinfo_p=p, pinfo_typ=ptyp} = pinfo
        val need_checks = pat_need_checks(p, ptyp)
        val have_vars = pat_have_vars(p)
        match (need_checks, have_vars) {
        | (true, false) => match p {
                           | PatLit(_, _) => (pinfo :: pl_c, pl_cu, pl_u)
                           | _ => (pl_c + (pinfo :: []), pl_cu, pl_u)
                           }
        | (true, true) => (pl_c, pinfo :: pl_cu, pl_u)
        | (false, true) => (pl_c, pl_cu, pinfo :: pl_u)
        | _ => (pl_c, pl_cu, pl_u)
        }
    }

    fun get_extract_tag_exp(a: atom_t, atyp: ktyp_t, loc: loc_t) =
        match deref_ktyp(atyp, loc) {
        | KTypExn => KExpIntrin(IntrinVariantTag, a :: [], (KTypCInt, loc))
        | KTypRecord(_, _) => KExpAtom(AtomLit(KLitInt(0L)), (KTypCInt, loc))
        | KTypName(tn) =>
            match kinfo_(tn, loc) {
            | KVariant (ref {kvar_cases}) =>
                match kvar_cases {
                | (n, _) :: [] =>
                    KExpAtom(AtomLit(KLitInt(1L)), (KTypCInt, loc))
                    //KExpIntrin(IntrinVariantTag, a :: [], (KTypCInt, loc))
                | _ => KExpIntrin(IntrinVariantTag, a :: [], (KTypCInt, loc))
                }
            | _ => throw compile_err(loc,
                f"k-normalize: enxpected type '{tn}'; record, variant of exception is expected here")
            }
        | t => throw compile_err(loc,
            f"k-normalize: enxpected type '{t}'; record, variant of exception is expected here")
        }

    fun process_next_subpat(plists: (pat_info_t list, pat_info_t list, pat_info_t list),
                            (checks: kcode_t, code: kcode_t), case_sc: scope_t list)
    {
        val temp_prefix = "v"
        fun process_pat_list(tup_id: id_t, pti_l: (pat_t, ktyp_t, int) list,
            plists: (pat_info_t list, pat_info_t list, pat_info_t list), alt_ei_opt: kexp_t?) =
            match pti_l {
            | (PatAny _, _, _) :: [] => plists
            | _ =>
                val fold plists_delta = [] for (pi, ti, idxi)@idx <- pti_l {
                    val loci = get_pat_loc(pi)
                    val ei =
                        match alt_ei_opt {
                        | Some(ei) =>
                            if idx != 0 {
                                throw compile_err(loci, "a code for singe-argument variant case \
                                    handling is used with a case with multiple patterns")
                            }
                            ei
                        | _ => KExpMem(tup_id, idxi, (ti, loci))
                        }
                    val pinfo_i = pat_info_t {pinfo_p=pi, pinfo_typ=ti, pinfo_e=ei, pinfo_tag=noid}
                    (pinfo_i :: plists_delta)
                }
                val fold plists = plists for pinfo <- plists_delta {
                    dispatch_pat(pinfo, plists)
                }
                plists
            }

        fun get_var_tag_cmp_and_extract(n: id_t, pinfo: pat_info_t, (checks: kcode_t, code: kcode_t),
            vn: id_t, sc: scope_t list, loc: loc_t)
        {
            val {pinfo_tag=var_tag0, pinfo_typ} = pinfo
            val (c_args, vn_tag_val, vn_case_val) =
            match kinfo_(vn, loc) {
            | KFun (ref {kf_params, kf_flags}) =>
                val c_args = [for arg <- kf_params {get_kval(arg, loc).kv_typ} ]
                val ctor = kf_flags.fun_flag_ctor
                val vn_val = match ctor {
                             | CtorVariant(tv) => AtomLit(KLitInt(int64(tv)))
                             | _ => AtomLit(KLitInt(1L))
                             }
                (c_args, vn_val, vn_val)
            | KExn (ref {ke_typ, ke_tag}) =>
                (match ke_typ {
                 | KTypTuple(args) => args
                 | KTypVoid => []
                 | _ => ke_typ :: []
                 }, AtomId(ke_tag), AtomId(vn))
            | KVal ({kv_flags}) =>
                val ctor_id = kv_flags.val_flag_ctor
                val vn_val = AtomLit(KLitInt(int64(ctor_id)))
                ([], vn_val, vn_val)
            | k => throw compile_err(loc, f"a variant constructor ('{vn}') is expected here")
            }
            val (tag_n, code) =
            if var_tag0 != noid {
                (var_tag0, code)
            } else {
                val tag_n = gen_idk(km_idx, "tag")
                val extract_tag_exp = get_extract_tag_exp(AtomId(n), pinfo_typ, loc)
                val code = create_kdefval(tag_n, KTypCInt, default_tempval_flags(),
                                          Some(extract_tag_exp), code, loc)
                (tag_n, code)
            }
            val cmp_tag_exp = KExpBinary(OpCmp(CmpEQ), AtomId(tag_n), vn_tag_val, (KTypBool, loc))
            val checks = rcode2kexp(cmp_tag_exp :: code, loc) :: checks
            val (case_n, code, alt_e_opt) =
            match c_args {
            | [] => (noid, [], None)
            | _ =>
                val (is_tuple, case_typ) = match c_args {
                                           | t :: [] => (false, t)
                                           | _ => (true, KTypTuple(c_args))
                                           }
                val extract_case_exp = KExpIntrin(IntrinVariantCase,
                    AtomId(n) :: vn_case_val :: [], (case_typ, loc))
                if is_tuple {
                    val case_n = gen_idk(km_idx, "vcase")
                    val code = create_kdefval(case_n, case_typ, default_tempref_flags(),
                                            Some(extract_case_exp), [], loc)
                    (case_n, code, None)
                } else {
                    (noid, [], Some(extract_case_exp))
                }
            }
            (case_n, c_args, checks, code, alt_e_opt)
        }

        val (p_opt, plists) =
        match plists {
        | (p :: pl_c, pl_cu, pl_u) => (Some(p), (pl_c, pl_cu, pl_u))
        | ([], p :: pl_cu, pl_u) => (Some(p), ([], pl_cu, pl_u))
        | ([], [], p :: pl_u) => (Some(p), ([], [], pl_u))
        | _ => (None, ([], [], []))
        }
        match p_opt {
        | Some(pinfo) =>
            val {pinfo_p=p, pinfo_typ=ptyp, pinfo_e=ke, pinfo_tag=var_tag0} = pinfo
            val (p, n, tref, _) = pat_propose_id(p, ptyp, temp_prefix, false, false, case_sc)
            if n == noid {
                process_next_subpat(plists, (checks, code), case_sc)
            } else {
                val loc = get_pat_loc(p)
                val (n, code) =
                match (ke, tref) {
                | (KExpAtom(AtomId(n0), _), true) => (n0, code)
                | _ =>
                    val flags =
                        if is_ktyp_scalar(ptyp) ||
                            (match ke { | KExpUnary(OpDeref, _, _) => true | _ => false }) {
                            default_tempval_flags()
                        } else {
                            default_tempref_flags()
                        }
                    val code = create_kdefval(n, ptyp, flags, Some(ke), code, loc)
                    (n, code)
                }
                val (plists, checks, code) =
                match p {
                | PatLit(l, _) =>
                    val code = KExpBinary(OpCmp(CmpEQ), AtomId(n),
                                    AtomLit(lit2klit(l, ptyp, loc)),
                                    (KTypBool, loc)) :: code
                    val c_exp = rcode2kexp(code, loc)
                    (plists, c_exp :: checks, [])
                | PatIdent(_, _) => (plists, checks, code)
                | PatCons(p1, p2, _) =>
                    val code = KExpBinary(OpCmp(CmpNE), AtomId(n),
                        AtomLit(KLitNil(ptyp)), (KTypBool, loc)) :: code
                    val c_exp = rcode2kexp(code, loc)
                    val et = match ptyp {
                             | KTypList(et) => et
                             | _ => throw compile_err(loc, "the pattern needs list type")
                             }
                    val get_hd_exp = KExpIntrin(IntrinListHead, AtomId(n) :: [], (et, loc))
                    val get_tl_exp = KExpIntrin(IntrinListTail, AtomId(n) :: [], (ptyp, loc))
                    val p_hd = pat_info_t {pinfo_p=p1, pinfo_typ=et, pinfo_e=get_hd_exp, pinfo_tag=noid}
                    val p_tl = pat_info_t {pinfo_p=p2, pinfo_typ=ptyp, pinfo_e=get_tl_exp, pinfo_tag=noid}
                    val plists = dispatch_pat(p_hd, plists)
                    val plists = dispatch_pat(p_tl, plists)
                    (plists, c_exp :: checks, [])
                | PatTuple(pl, loc) =>
                    val tl = match ptyp {
                             | KTypTuple(tl) => tl
                             | _ => throw compile_err(loc,
                                "invalid type of the tuple pattern (it must be a tuple as well)")
                             }
                    val fold pti_l = [] for pi@idx <- pl, ti <- tl { (pi, ti, idx) :: pti_l }
                    val plists = process_pat_list(n, pti_l, plists, None)
                    (plists, checks, code)
                | PatVariant(vn, pl, loc) =>
                    val (case_n, tl, checks, code, alt_e_opt) =
                        get_var_tag_cmp_and_extract(n, pinfo, (checks, code), vn, case_sc, loc)
                    val plists =
                    if case_n == noid && alt_e_opt.isnone() {
                        plists
                    } else {
                        val fold pti_l = [] for pi@idx <- pl, ti <- tl { (pi, ti, idx) :: pti_l }
                        process_pat_list(case_n, pti_l, plists, alt_e_opt)
                    }
                    match_var_cases = match_var_cases.remove(get_orig_id(vn))
                    (plists, checks, code)
                | PatRecord(rn_opt, _, loc) =>
                    val (case_n, _, checks, code, alt_e_opt) =
                    match rn_opt {
                    | Some(rn) =>
                        match_var_cases = match_var_cases.remove(get_orig_id(rn))
                        get_var_tag_cmp_and_extract(n, pinfo, (checks, code), rn, case_sc, loc)
                    | _ => (n, [], checks, code, None)
                    }
                    val plists =
                    if case_n == noid && alt_e_opt.isnone() {
                        plists
                    } else {
                        val (_, ktyp_rec_pl) = match_record_pat(p, ptyp)
                        val pti_l = [for (_, pi, ti, idxi) <- ktyp_rec_pl { (pi, ti, idxi) } ]
                        process_pat_list(case_n, pti_l, plists, alt_e_opt)
                    }
                    (plists, checks, code)
                | PatAs(p, _, _) =>
                    val pinfo = pat_info_t {pinfo_p=p, pinfo_typ=ptyp,
                        pinfo_e=KExpAtom(AtomId(n), (ptyp, loc)), pinfo_tag=var_tag0}
                    val plists = dispatch_pat(pinfo, plists)
                    (plists, checks, code)
                | PatRef(p, _) =>
                    val t = match ptyp {
                            | KTypRef(t) => t
                            | _ => throw compile_err(loc, "the ref() pattern needs reference type")
                            }
                    val get_val = KExpUnary(OpDeref, AtomId(n), (t, loc))
                    val pinfo_p = pat_info_t {pinfo_p=p, pinfo_typ=t, pinfo_e=get_val, pinfo_tag=noid}
                    val plists = dispatch_pat(pinfo_p, plists)
                    (plists, checks, code)
                | PatWhen(p, e, _) =>
                    val pinfo = pat_info_t {pinfo_p=p, pinfo_typ=ptyp,
                        pinfo_e=KExpAtom(AtomId(n), (ptyp, loc)), pinfo_tag=var_tag0}
                    val plists = dispatch_pat(pinfo, plists)
                    val (checks, code) = process_next_subpat(plists, (checks, code), case_sc)
                    val (ke, code) = exp2kexp(e, code, true, sc)
                    val c_exp = rcode2kexp(ke :: code, loc)
                    (([], [], []), c_exp :: checks, [])
                | PatAlt(pl, _) =>
                    if pat_have_vars(p) {
                        throw compile_err(loc, "alt-pattern cannot contain captured values")
                    }
                    // build alt_checks, a list of expression lists, which are supposed to be combined by || operator.
                    val fold alt_cases = ([], KExpAtom(AtomLit(KLitBool(false)), (KTypBool, loc)))::[] for p <- pl.rev() {
                        val pinfo = pat_info_t {pinfo_p=p, pinfo_typ=ptyp,
                            pinfo_e=KExpAtom(AtomId(n), (ptyp, loc)), pinfo_tag=var_tag0}
                        val plists_ = dispatch_pat(pinfo, ([], [], []))
                        val (checks_, code_) = process_next_subpat(plists_, ([], []), case_sc)
                        val e = rcode2kexp(KExpAtom(AtomLit(KLitBool(true)), (KTypBool, loc)) :: code_, loc)
                        (checks_.rev(), e) :: alt_cases
                    }
                    val alt_check = rcode2kexp(KExpMatch(alt_cases, (KTypBool, loc)) :: code, loc)
                    (plists, alt_check :: checks, [])
                | _ => throw compile_err(loc, "this type of pattern is not supported yet")
                }
                process_next_subpat(plists, (checks, code), case_sc)
            }
        | _ => (checks, code)
        }
    }

    val atyp = get_atom_ktyp(a, loc)
    val is_variant =
    match atyp {
    | KTypExn => true
    | KTypName(tname) =>
        match kinfo_(tname, loc) {
        | KVariant (ref {kvar_cases}) =>
            match_var_cases = fold match_var_cases=empty_idset for (n, _) <- kvar_cases {
                match_var_cases.add(get_orig_id(n))
            }
            true
        | _ => false
        }
    | _ => false
    }
    val (var_tag0, code) =
    if !is_variant {
        (noid, code)
    } else {
        val tag_n = gen_idk(km_idx, "tag")
        val extract_tag_exp = get_extract_tag_exp(a, atyp, loc)
        val code = create_kdefval(tag_n, KTypCInt, default_tempval_flags(),
                                  Some(extract_tag_exp), code, loc)
        (tag_n, code)
    }
    var have_else = false
    val k_cases =
    [for (p, e) <- cases {
        val ploc = get_pat_loc(p)
        val pinfo = pat_info_t {pinfo_p=p, pinfo_typ=atyp,
            pinfo_e=KExpAtom(a, (atyp, loc)), pinfo_tag=var_tag0}
        if have_else {
            throw compile_err(ploc, "unreacheable pattern matching case")
        }
        val plists = dispatch_pat(pinfo, ([], [], []))
        val case_sc = new_block_scope(km_idx) :: sc
        val (checks, case_code) = process_next_subpat(plists, ([], []), case_sc)
        val (ke, case_code) = exp2kexp(e, case_code, false, case_sc)
        val eloc = get_exp_loc(e)
        val ke = rcode2kexp(ke :: case_code, eloc)
        if checks == [] { have_else = true }
        (checks.rev(), ke)
    }]
    if is_variant && !have_else && !match_var_cases.empty() {
        val idlist = ", ".join(match_var_cases.map(fun (n) {f"'{n}'"}))
        val idlist_len = idlist.length()
        val msg = if idlist_len == 1 {f"the case {idlist} is not covered"}
                  else {f"the cases [{idlist}] are not covered"}
        throw compile_err(loc, f"{msg}; add '| _ => ...' clause to suppress this error")
    }
    val k_cases =
        if have_else {
            k_cases
        } else if catch_mode {
            val rethrow_exp = KExpThrow(atom2id(a, loc,
                "internal error: a literal cannot occur here"), true, loc)
            k_cases + [([], rethrow_exp) ]
        } else {
            if builtin_exn_NoMatchError == noid {
                throw compile_err(loc, "internal error: NoMatchError exception is not found")
            }
            val nomatch_err = KExpThrow(builtin_exn_NoMatchError, false, loc)
            k_cases + [([], nomatch_err) ]
        }
    (k_cases, code)
}

fun transform_fun(df: deffun_t ref, code: kcode_t, sc: scope_t list): kcode_t
{
    val km_idx = curr_module(sc)
    val {df_name, df_templ_args, df_templ_inst, df_loc} = *df
    val is_private_fun =
        match sc {
        | [] | ScModule _ :: _ => false
        | _ => true
        }
    val inst_list = if df_templ_args == [] { df_name :: [] } else { *df_templ_inst }
    fold code = code for inst <- inst_list {
        match id_info(inst, df_loc) {
        | IdFun(inst_df) =>
            val {df_name=inst_name, df_args=inst_args, df_typ=inst_typ,
                 df_body=inst_body, df_flags=inst_flags, df_loc=inst_loc} = *inst_df
            val ktyp = typ2ktyp(inst_typ, df_loc)
            val (argtyps, rt) =
                match ktyp {
                | KTypFun(argtyps, rt) => (argtyps, rt)
                | _ => throw compile_err(inst_loc,
                    f"the type of non-constructor function '{inst_name}' should be TypFun(_,_)")
                }
            val (inst_args, argtyps, inst_body) =
            if !inst_flags.fun_flag_have_keywords {
                (inst_args, argtyps, inst_body)
            } else {
                match (inst_args.rev(), argtyps.rev(), inst_body) {
                | (_ :: rest_inst_args, KTypRecord(_, relems) :: rest_argtyps,
                      ExpSeq(DefVal(PatRecord(_, relems_pats, _),
                      ExpIdent(_, _), _, loc) :: rest_inst_body, body_ctx)) =>
                    if relems.length() != relems_pats.length() {
                        throw compile_err(loc,
                            "the number of pattern elems in the unpack operation is incorrect")
                    }
                    if rest_argtyps.length() != rest_inst_args.length() {
                        throw compile_err(loc,
                            "the number of positional arguments and their types do not match")
                    }
                    val fold (inst_args, argtyps) = (rest_inst_args, rest_argtyps)
                        for (ni, ti) <- relems, (ni_, pi) <- relems_pats {
                            if ni != ni_ {
                                throw compile_err(loc, f"the record field '{ni}' \
                                    does not match the record pattern field '{ni_}'")
                            }
                            (pi :: inst_args, ti :: argtyps)
                        }
                    val new_inst_body = match rest_inst_body {
                        | [] => ExpNop(df_loc)
                        | e :: [] => e
                        | _ => ExpSeq(rest_inst_body, body_ctx)
                        }
                    (inst_args.rev(), argtyps.rev(), new_inst_body)
                | _ =>
                    throw compile_err( df_loc,
                        "the function with keyword parameters must have the anonymous record as \
                        the last parameter and should start with the record unpacking")
                }
            }
            val nargs = inst_args.length()
            val nargtypes = argtyps.length()
            if nargs != nargtypes {
                throw compile_err(inst_loc, f"the number of argument patterns ({nargs}) and \
                                the number of argument types ({nargtypes}) do not match")
            }
            val body_sc = new_block_scope(km_idx) :: sc
            val fold (params, body_code) = ([], []) for pi@idx <- inst_args, ti <- argtyps {
                    val arg_defname = f"arg{idx}"
                    val (i, body_code) = pat_simple_unpack(pi, ti, None, body_code, arg_defname,
                                                            default_val_flags(), body_sc)
                    val i = if i.m == 0 {dup_idk(km_idx, i)} else {i}
                    val _ = create_kdefval(i, ti, default_arg_flags(), None, [], inst_loc)
                    (i :: params, body_code)
                }
            //print(f"func {inst_name} body: "); Ast_pp.pprint_exp_x(inst_body); println()
            val is_cfunc = match inst_body {
                | ExpCCode(_, _) | ExpSeq(_ :: ExpCCode(_, _) :: [], _) => true
                | _ => false }
            val inst_flags = inst_flags.{
                fun_flag_ccode=is_cfunc,
                fun_flag_nothrow=inst_flags.fun_flag_nothrow && is_cfunc,
                fun_flag_private=is_private_fun
                }
            val _ = create_kdeffun(inst_name, params.rev(), rt, inst_flags, None, code, sc, inst_loc)
            val body_loc = get_exp_loc(inst_body)
            val (e, body_code) = exp2kexp(inst_body, body_code, false, body_sc)
            val body_kexp = rcode2kexp(e :: body_code, body_loc)
            create_kdeffun(inst_name, params.rev(), rt, inst_flags, Some(body_kexp), code, sc, inst_loc)
        | i =>
            throw compile_err( get_idinfo_loc(i),
                f"the entry '{inst}' (an instance of '{df_name}'?) \
                  is supposed to be a function, but it's not")
        }
    }
}

fun transform_all_types_and_cons(elist: exp_t list, code: kcode_t, sc: scope_t list): kcode_t
{
    val km_idx = curr_module(sc)
    fold code = code for e <- elist {
        | DefVariant (ref {dvar_name, dvar_templ_args, dvar_cases, dvar_templ_inst, dvar_scope, dvar_loc}) =>
            val inst_list = if dvar_templ_args == [] { dvar_name :: [] } else { *dvar_templ_inst }
            val tags =
                [for (n, _)@i <- dvar_cases {
                    if n == noid { noid } else {
                        val tag_id = dup_idk(km_idx, n)
                        val tag_flags = default_val_flags().{val_flag_global=dvar_scope}
                        val _ = create_kdefval(tag_id, KTypCInt, tag_flags,
                            Some(KExpAtom(AtomLit(KLitSInt(32, int64(i+1))), (KTypCInt, dvar_loc))),
                            [], dvar_loc)
                        tag_id
                    }
                }]
            fold code = code for inst <- inst_list {
                match id_info(inst, dvar_loc) {
                | IdVariant (ref {dvar_name=inst_name, dvar_alias=inst_alias, dvar_cases,
                                  dvar_ctors, dvar_ifaces, dvar_flags, dvar_scope, dvar_loc=inst_loc}) =>
                    val targs =
                    match deref_typ(inst_alias) {
                    | TypApp(targs, _) => [for t <- targs { typ2ktyp(t, inst_loc) } ]
                    | _ => throw compile_err(inst_loc,
                        f"invalid variant type alias '{inst_name}'; should be TypApp(_, _)")
                    }
                    match (dvar_cases, dvar_flags.var_flag_record, dvar_ifaces) {
                    | ((rn, TypRecord (ref (relems, _))) :: [], true, []) =>
                        val rec_elems = [for (_, i, t, _) <- relems { (i, typ2ktyp(t, inst_loc)) } ]
                        val kt = ref (kdeftyp_t { kt_name=inst_name, kt_cname="",
                                kt_targs=targs, kt_proto=noid, kt_props=None,
                                kt_typ=KTypRecord(inst_name, rec_elems),
                                kt_scope=sc, kt_loc=inst_loc })
                        set_idk_entry(inst_name, KTyp(kt))
                        KDefTyp(kt) :: code
                    | _ =>
                        val kvar_cases = [for (_, t) <- dvar_cases, tag <- tags {
                                            (tag, typ2ktyp(t, inst_loc)) }]
                        val kvar_ifaces = [for (iname, meths) <- dvar_ifaces {
                            (iname, [for (a, b) <- meths {
                                if b == noid {
                                    throw compile_err(inst_loc,
                                    f"type '{pp(dvar_name)} claims to implement interface '{pp(iname)}', " +
                                    f"but the method '{pp(iname)}.{pp(a)}' is not implemented")
                                }
                                b
                            }]) }]
                        val kvar = ref (kdefvariant_t {
                            kvar_name=inst_name, kvar_cname="", kvar_targs=targs,
                            kvar_proto=noid, kvar_props=None, kvar_cases=kvar_cases,
                            kvar_ctors=dvar_ctors, kvar_ifaces=kvar_ifaces,
                            kvar_flags=dvar_flags, kvar_scope=sc, kvar_loc=inst_loc
                            })
                        set_idk_entry(inst_name, KVariant(kvar))
                        val code = KDefVariant(kvar) :: code
                        val new_rt = KTypName(inst_name)
                            fold code=code for ctor@i <- dvar_ctors, tag <- tags {
                                match id_info(ctor, dvar_loc) {
                                | IdFun (ref {df_name, df_typ}) =>
                                    val argtyps = match df_typ {
                                        | TypFun(TypRecord (ref (relems, true)) :: [], _) =>
                                            [for (_, n, t, _) <- relems {t} ]
                                        | TypFun(argtyps, _) => argtyps
                                        | _ => []
                                        }
                                    val kargtyps = [for t <- argtyps {typ2ktyp(t, dvar_loc)} ]
                                    val tagval = i+1
                                    val code =
                                        match kargtyps {
                                        | [] =>
                                            val e0 = KExpAtom(AtomId(tag), (new_rt, dvar_loc))
                                            val cflags = default_val_flags().{val_flag_global=sc,
                                                            val_flag_instance=dvar_flags.var_flag_instance,
                                                            val_flag_mutable=true, val_flag_ctor=tagval}
                                            create_kdefval(df_name, new_rt, cflags, Some(e0), code, dvar_loc)
                                        | _ =>
                                            create_kdefconstr(df_name, kargtyps, new_rt, CtorVariant(tagval),
                                                              dvar_flags.var_flag_instance, code, sc, dvar_loc)
                                        }
                                    code
                                | _ =>
                                    throw compile_err(dvar_loc,
                                        f"the constructor '{ctor}' of variant '{inst}' \
                                        is not a function apparently")
                                }
                            }
                    }
                | _ =>
                    throw compile_err(dvar_loc,
                        f"the instance '{inst}' of variant '{dvar_name}' is not a variant")
                }
            }
        | DefExn (ref {dexn_name, dexn_typ, dexn_loc, dexn_scope}) =>
            val is_std =
            match (dexn_scope, deref_typ(dexn_typ)) {
            | (ScModule(m) :: _, TypVoid) when pp(get_module_name(m)) == "Builtins" =>
                val exn_name_str = pp(dexn_name)
                if exn_name_str == "OutOfRangeError" {
                    builtin_exn_OutOfRangeError = dexn_name
                } else if exn_name_str == "NoMatchError" {
                    builtin_exn_NoMatchError = dexn_name
                }
                true
            | _ => false
            }
            val tagname = gen_idk(km_idx, pp(dexn_name) + "_tag")
            val tag_sc = get_module_scope(sc)
            val tag_flags = default_val_flags().{val_flag_global=tag_sc, val_flag_mutable=true}
            val decl_tag = create_kdefval(tagname, KTypCInt, tag_flags,
                Some(KExpAtom(AtomLit(KLitInt(0L)), (KTypInt, dexn_loc))), [], dexn_loc)
            val code = if is_std { code } else { decl_tag + code }
            val dexn_typ =
                match deref_typ(dexn_typ) {
                | TypRecord (ref (relems, true)) => TypTuple([for (_, _, t, _) <- relems {t} ])
                | _ => dexn_typ
                }
            val ke_typ = typ2ktyp(dexn_typ, dexn_loc)
            val (make_id, delta_code) =
            match ke_typ {
            | KTypVoid => (noid, [])
            | _ =>
                val make_id = gen_idk(km_idx, "make_" + pp(dexn_name))
                val argtyps = match ke_typ {
                              | KTypTuple(telems) => telems
                              | _ => ke_typ :: []
                              }
                val delta_code = create_kdefconstr(make_id, argtyps, KTypExn,
                                    CtorExn(dexn_name), false, [], dexn_scope, dexn_loc)
                (make_id, delta_code)
            }
            val ke = ref (kdefexn_t {
                ke_name=dexn_name, ke_cname="",
                ke_base_cname="", ke_typ=ke_typ,
                ke_std=is_std, ke_tag=tagname,
                ke_make=make_id, ke_scope=sc,
                ke_loc=dexn_loc
                })
            set_idk_entry(dexn_name, KExn(ke))
            delta_code + (KDefExn(ke) :: code)
        | DefInterface (ref {di_name, di_base, di_all_methods, di_scope, di_loc}) =>
            val ki_all_methods =
                [for (f, t, _) <- di_all_methods {
                    val ktyp = typ2ktyp(t, di_loc)
                    val dv = match id_info(f, di_loc) {
                        | IdDVal(dv) => dv
                        | _ => throw compile_err(di_loc,
                            f"description of method '{pp(di_name)}.{pp(f)}' is not found")
                        }
                    val _ = create_kdefval(f, ktyp, dv.dv_flags, None, [], di_loc)
                    (f, ktyp)
                }]
            val ki_id = gen_idk(km_idx, pp(di_name) + "_id")
            val code = create_kdefval(ki_id, KTypCInt,
                default_var_flags().{val_flag_global=di_scope},
                Some(KExpAtom(AtomLit(KLitInt(-1L)), (KTypCInt, di_loc))), code, di_loc)
            val ki = ref (kdefinterface_t {
                ki_name = di_name,
                ki_base = di_base,
                ki_id = ki_id,
                ki_cname = "",
                ki_all_methods = ki_all_methods,
                ki_scope = di_scope,
                ki_loc = di_loc
                })
            set_idk_entry(di_name, KInterface(ki))
            KDefInterface(ki) :: code
        | _ => code
        }
}

fun embed_data(kind: string, fname: string, (_: ktyp_t, loc: loc_t))
{
    if kind == "text" {
        try {
            val text = File.read_utf8(fname)
            KExpAtom(AtomLit(KLitString(text)), (KTypString, loc))
        } catch {
            | _ => throw compile_err(loc, f"@text: {fname} cannot be read")
        }
    } else if kind == "binary" {
        val data = File.read_binary_u8(fname)
        val data = [for i <- data {(false, AtomLit(KLitUInt(8, uint64(i))))} ]
        KExpMkArray(true, data :: [], (KTypArray(1, KTypUInt(8)), loc))
    } else {
        throw compile_err(loc, f"'{kind}' data is not supported")
    }
}

fun normalize_mod(minfo: defmodule_t, kcode_typedefs: kcode_t, toposort_idx: int, is_main: bool): kmodule_t
{
    idx_access_stack = []
    val modsc = ScModule(minfo.dm_idx) :: []
    val (kcode, pragmas) = eseq2code(minfo.dm_defs, kcode_typedefs, modsc)
    kmodule_t { km_name=minfo.dm_name, km_idx=minfo.dm_idx, km_cname=pp(minfo.dm_name),
                km_top=kcode.rev(), km_deps=minfo.dm_deps, km_toposort_idx=toposort_idx, km_main=is_main,
                km_skip=false, km_pragmas=parse_pragmas(pragmas) }
}

fun normalize_all_modules(modules: int list): kmodule_t list
{
    val n = modules.length()
    val fold modules_plus = [] for m <- modules {
        val minfo = get_module(m)
        val modsc = ScModule(m) :: []
        val kcode_typedefs = transform_all_types_and_cons(minfo.dm_defs, [], modsc)
        (minfo, kcode_typedefs) :: modules_plus
    }
    [for (minfo, kcode_typedefs)@i <- modules_plus.rev() {
        normalize_mod(minfo, kcode_typedefs, i, i + 1 == n)
    }]
}
