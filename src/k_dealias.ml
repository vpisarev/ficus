(*
    performs dealiasing of K-form. That is,
    we look for the trivial assignments, such as
        * `val a@34535=b@795` where `b@795` is immutable value,
        * or `val a@34535=3.1415` constructions
    and then remove this value definition and replace everywhere
    `a@34535` with `b@795`/`3.1415`, respectively.
*)
