(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Syntax of (a subset of) OCaml *)

exception SyntaxError of string * Lexing.position*Lexing.position

type id_t = string
let noid = ""

type oclit_t =
| ClUnit
| ClBool of bool
| ClInt of int
| ClFloat of float
| ClString of string
| ClChar of char
| ClNil

type octyp_t =
| CtUnit
| CtBool
| CtInt
| CtFloat
| CtString
| CtChar
| CtList of octyp_t
| CtRef of octyp_t
| CtTuple of octyp_t list
(* [mutable1?] v1: t1; [mutable2?] v2: t2; ... *)
| CtRecord of (bool * id_t * octyp_t) list
| CtApp of octyp_t list * id_t
| CtFun of octyp_t * octyp_t
| CtName of id_t

type uop_t = COpNeg | COpNot | COpDeref | COpMkRef
type bop_t =
    | COpAdd | COpSub | COpMul | COpDiv
    | COpMod | COpLogicAnd | COpLogicOr | COpConcat
    | COpEQ | COpNE | COpLT | COpLE | COpGE | COpGT
    | COpSHL | COpSHR | COpCons | COpAssign | COpAt

type ocexp_t =
    | CeLit of oclit_t
    | CeUnary of uop_t * ocexp_t
    | CeBinary of bop_t * ocexp_t * ocexp_t
    | CeIf of ocexp_t * ocexp_t * ocexp_t
    | CeLet of ocpat_t * ocexp_t * ocexp_t option
    | CeLetRec of (ocfundef_t list * ocexp_t option)
    | CeLambda of ocpat_t list * ocexp_t
    | CeLambdaCases of (ocpat_t list * ocexp_t) list
    | CeIdent of id_t
    | CeTyped of ocexp_t * octyp_t
    | CeCall of ocexp_t * ocexp_t list
    | CeMkTuple of ocexp_t list
    | CeMkList of ocexp_t list
    | CeMkVector of ocexp_t list
    | CeMkRecord of (id_t * ocexp_t) list
    | CeUpdateRecord of ocexp_t * (id_t * ocexp_t) list
    | CeMatch of ocexp_t * (ocpat_t list * ocexp_t) list
    | CeTry of ocexp_t * (ocpat_t list * ocexp_t) list
    | CeRaise of ocexp_t
    | CeWhile of ocexp_t * ocexp_t
    | CeFor of bool * id_t * ocexp_t * ocexp_t * ocexp_t
    | CeBlock of ocexp_t list
    | CeDefTyp of octvdef_t list
    | CeDefExn of id_t * octyp_t
    | CeOpen of id_t list
and ocpat_t =
    | CpLit of oclit_t
    | CpIdent of id_t
    | CpAny
    | CpTuple of ocpat_t list
    | CpVariant of id_t * ocpat_t list
    | CpRecord of id_t * (id_t * ocpat_t) list
    | CpCons of ocpat_t * ocpat_t
    | CpAs of ocpat_t * id_t
    | CpWhen of ocpat_t * ocexp_t
    | CpTyped of ocpat_t * octyp_t
and octvdef_t =
    | DefTyp of octypdef_t
    | DefVariant of ocvardef_t
and ocfundef_t = { ocf_name: id_t; ocf_args: ocpat_t list; ocf_body : ocexp_t }
and octypdef_t = { oct_name: id_t; oct_args: id_t list; oct_body: octyp_t }
and ocvardef_t = { ocv_name: id_t; ocv_args: id_t list; ocv_cases: (id_t * octyp_t) list }

let opt_infer_types = ref false

let good_variant_name s =
    let c0 = String.get s 0 in
    ('A' <= c0 && c0 <= 'Z') || (String.contains s '.')

let printf = Format.printf
let sprintf = Format.sprintf
