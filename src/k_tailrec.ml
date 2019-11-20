(*
    Transforms tail-recursive functions into loops.

    Note that certain functions may call itself several times,
    some of the calls may be tail calls whereas the others may be
    non-tail calls. A classical example is qsort. In those cases
    the algorithm retains non-tail calls as recursive calls and
    transforms the function body in such a way that all tail calls
    update the parameters and 'jump' into the beginning of the function.
*)
