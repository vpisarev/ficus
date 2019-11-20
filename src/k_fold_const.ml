(*
    perform "constant folding" optimization step of K-form:
    * `1+2` is replaced with `3` etc.
    * `a+0, a-0, a*1, a/1` is replaced with `a`
    * `a-a, a*0, 0*a` is replaced with `0`
    * `a & true, true & a, a | false, false | a` is replaced with a
    * `a & false, false & a` is replaced with false
*)
