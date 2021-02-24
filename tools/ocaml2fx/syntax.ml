(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Syntax of (a subset of) OCaml *)

exception SyntaxError of string * Lexing.position*Lexing.position

type id_t = string
let noid = ""

type oclit_t =
| LUnit
| LBool of bool
| LInt of int
| LInt64 of int
| LFloat of float
| LString of string
| LChar of char
| LNil

type octyp_t =
| TUnit
| TBool
| TInt
| TFloat
| TString
| TChar
| TList of octyp_t
| TRef of octyp_t
| TTuple of octyp_t list
(* [mutable1?] v1: t1; [mutable2?] v2: t2; ... *)
| TRecord of (bool * id_t * octyp_t) list
| TApp of octyp_t list * id_t
| TFun of octyp_t * octyp_t
| TName of id_t

type uop_t = OpNeg | OpNot | OpDeref | OpMkRef
type bop_t =
    | OpAdd | OpSub | OpMul | OpDiv
    | OpMod | OpLogicAnd | OpLogicOr | OpConcat
    | OpEQ | OpNE | OpLT | OpLE | OpGE | OpGT
    | OpSHL | OpSHR | OpCons | OpAssign | OpAt | OpMem

type ocexp_t =
    | ELit of oclit_t
    | EUnary of uop_t * ocexp_t
    | EBinary of bop_t * ocexp_t * ocexp_t
    | EIf of ocexp_t * ocexp_t * ocexp_t
    | ELet of ocpat_t * ocexp_t * ocexp_t option
    | ELetRec of (ocfundef_t list * ocexp_t option)
    | ELambda of ocpat_t list * ocexp_t
    | ELambdaCases of (ocpat_t list * ocexp_t) list
    | EIdent of id_t
    | ETyped of ocexp_t * octyp_t
    | ECall of ocexp_t * ocexp_t list
    | EMkTuple of ocexp_t list
    | EMkList of ocexp_t list
    | EMkVector of ocexp_t list
    | EMkRecord of (id_t * ocexp_t) list
    | EUpdateRecord of ocexp_t * (id_t * ocexp_t) list
    | EMatch of ocexp_t * (ocpat_t list * ocexp_t) list
    | ETry of ocexp_t * (ocpat_t list * ocexp_t) list
    | ERaise of ocexp_t
    | EWhile of ocexp_t * ocexp_t
    | EFor of bool * id_t * ocexp_t * ocexp_t * ocexp_t
    (*| EForEach of ocpat_t * ocexp_t * ocexp_t
    | EMap of ocpat_t * ocexp_t * ocexp_t
    | EFold of (ocpat_t * ocexp_t) * (ocpat_t * ocexp_t) * ocexp_t*)
    | EBlock of ocexp_t list
    | EDefTyp of octvdef_t list
    | EDefExn of id_t * octyp_t
    | EOpen of id_t list
and ocpat_t =
    | PLit of oclit_t
    | PIdent of id_t
    | PAny
    | PTuple of ocpat_t list
    | PVariant of id_t * ocpat_t list
    | PRecord of id_t * (id_t * ocpat_t) list
    | PCons of ocpat_t * ocpat_t
    | PAs of ocpat_t * id_t
    | PWhen of ocpat_t * ocexp_t
    | PTyped of ocpat_t * octyp_t
and octvdef_t =
    | DefTyp of octypdef_t
    | DefVariant of ocvardef_t
and ocfundef_t = { ocf_name: id_t; ocf_args: ocpat_t list; ocf_body : ocexp_t }
and octypdef_t = { oct_name: id_t; oct_args: id_t list; oct_body: octyp_t }
and ocvardef_t = { ocv_name: id_t; ocv_args: id_t list; ocv_cases: (id_t * octyp_t) list }

let opt_infer_types = ref false
let opt_pr_tokens = ref false
let lineno = ref 1

let good_variant_name s =
    let c0 = String.get s 0 in
    ('A' <= c0 && c0 <= 'Z') || (String.contains s '.')

let printf = Format.printf
let sprintf = Format.sprintf

let rec octyp2str t =
    match t with
    | TUnit -> "void"
    | TBool -> "bool"
    | TInt -> "int"
    | TFloat -> "double"
    | TString -> "string"
    | TChar -> "char"
    | TList t -> (octyp2str t) ^ " list"
    | TRef t -> (octyp2str t) ^ " ref"
    | TTuple(tl) -> "(" ^ (String.concat ", " (List.map octyp2str tl)) ^ ")"
    | TRecord(relems) -> "{" ^ (String.concat "; " (List.map (fun (m, n, t) ->
        let t = if m then TRef(t) else t in n ^ ": " ^ (octyp2str t)) relems)) ^ "}"
    | TApp(args, n) ->
        let argt = match args with t :: [] -> t | _ -> TTuple(args) in
        (octyp2str argt) ^ (if n = "option" then "?" else if n = "vector" then " []" else (" " ^ n))
    | TFun(arg, rt) -> "(" ^ (octyp2str arg) ^ " -> " ^ (octyp2str rt) ^ ")"
    | TName(n) -> n

let lit2str lit =
    match lit with
    | LInt(n) -> sprintf "%d" n
    | LInt64(n) -> sprintf "%dL" n
    | LFloat(f) -> sprintf "%.16g" f
    | LString(s) -> "\"" ^ s ^ "\""
    | LChar(c) -> "'" ^ (String.make 1 c) ^ "'"
    | LBool(b) -> if b then "true" else "false"
    | LNil -> "[]"
    | LUnit -> "{}"

let rec pat2str p =
    match p with
    | PLit(l) -> lit2str l
    | PIdent(n) -> n
    | PAny -> "<Any>"
    | PTuple(pl) -> "(" ^ (String.concat ", " (List.map pat2str pl)) ^ ")"
    | PVariant(vn, pl) ->
        vn ^ (pat2str (PTuple pl))
    | PRecord (vn, relems) ->
        vn ^ "{" ^ (String.concat ", " (List.map
            (fun (n, p) -> n ^ "=" ^ (pat2str p)) relems)) ^ "}"
    | PCons(p1, p2) -> "(" ^ (pat2str p1) ^ " :: " ^ (pat2str p2) ^ ")"
    | PAs(p, n) -> "(" ^ (pat2str p) ^ " as " ^ n ^ ")"
    | PWhen(p, n) -> "(" ^ (pat2str p) ^ " when ... )"
    | PTyped(p, t) -> "(" ^ (pat2str p) ^ ": " ^ (octyp2str t) ^ ")"
