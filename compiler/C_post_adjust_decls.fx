/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Separates declarations from initialization in some cases, e.g.

    {
    if (foo(x, y) < 0) goto catch;
    int a = 5;
    ...
  catch:
    ...
    }

    =>

    {
    int a;
    if (foo(x, y) < 0) goto catch;
    a = 5;
    ...
  catch:
    ...
    }

    this is necessary to shutup C++ compiler that reports error in such case,
    even though C compiler processes the same code just fine.
    the extra trick is applied to retain the array initializations as-is (we
    take care in c_gen_code.ml that such array initializations are enclosed into dedicated
    code blocks without jumps in the middle) and to retain declarations+initializations
    in the beginning of each code block.
*/

from Ast import *
from K_form import *
from C_form import *

fun adjust_decls(cmod: cmodule_t)
{
    val {cmod_ccode} = cmod
    var local_decls: cstmt_t list = []
    // whether we already have some operators,
    // not just declarations, in the current block
    var local_have_ops = false

    fun adjust_sseq(sseq: ccode_t, callb: c_callb_t)
    {
        val saved_decls = local_decls
        local_decls = []
        val sseq = [for s <- sseq { adjust_cstmt(s, callb) } ]
        val sseq = local_decls.rev() + sseq
        local_decls = saved_decls
        sseq
    }

    fun adjust_cstmt(s: cstmt_t, callb: c_callb_t) =
        match s {
        | CDefVal (t, n, Some e, loc)
            when local_have_ops &&
            (match t {| CTypRawArray _ => false | _ => true}) =>
            local_decls = CDefVal(t, n, None, loc) :: local_decls
            CExp(CExpBinary(COpAssign, make_id_t_exp(n, t, loc), e, (CTypVoid, loc)))
        | CDefVal _ | CDefForwardSym _ | CDefForwardTyp _ | CDefTyp _
        | CDefEnum _ | CMacroDef _ | CMacroUndef _ | CMacroIf _
        | CMacroInclude _ | CMacroPragma _ | CExp(CExpCCode _) =>
            s
        | CDefFun cf =>
            val {cf_body} = *cf
            val new_cf_body = adjust_sseq(cf_body, callb)
            *cf = cf->{cf_body=new_cf_body}
            local_have_ops = false
            s
        | _ =>
            local_have_ops = true
            match s {
            | CStmtBlock (sseq, loc) =>
                val sseq = adjust_sseq(sseq, callb)
                CStmtBlock(sseq, loc)
            | _ => walk_cstmt(s, callb)
            }
        }

    val adjust_callb = c_callb_t {
        ccb_ident=None,
        ccb_typ=None,
        ccb_exp=None,
        ccb_stmt=Some(adjust_cstmt)
    }
    val ccode = [for s <- cmod_ccode { adjust_cstmt(s, adjust_callb) } ]
    cmod.{cmod_ccode=ccode}
}

fun adjust_decls(cmods: cmodule_t list) = cmods.map(adjust_decls)
