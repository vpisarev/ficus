(*
    very simple variant of 'lambda-lifting' optimization step,
    which is performed once in the very beginning after the
    initial dead code elimination step.

    It does the following:
    * moves all the type definitions to the top level (i.e. global/module level).
    * moves all the exception declarations to the top level.
    * moves all the nested functions that do not access local variables/parameters
      of the outer functions to the top level. That is, those are functions that
      do not need closures. If a function calls functions or accesses values from
      the top/module level, it's not a problem, since it does not require a closure.
      In particular, this step moves C-code functions to the top level.

    Why is this step needed? This step is needed to simplify the
    inline function expansion step, i.e. increase the number of functions that
    can potentially be inlined. It also reduces the amount of work needed to
    be done by the full-scale lambda lifting step before translation to C/machine code.
*)
