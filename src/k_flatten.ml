(*
    try to move as much code as possible into the upper-level
    expression sequence without changing the semantics and
    without affecting possible side effects of the code. That is:
    * `val a = {b; c; d}` is replaced with `b; c; val a = d`
    * `if({a; b; c}) e1 else e2` is replaced with `a; b; if(c) e1 else e2`.
    etc.
    part of those transformations are done at the K-normalization step,
    but this is a dedicated step that can be useful together with other
    transformations to keep the code structure as 'flat' as possible
    (hence the name `flatten`).
*)
