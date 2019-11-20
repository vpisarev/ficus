(*
    remove unused code from the K-form:
    * any type or exception that is declared but not referenced.
      that includes the types that reference itself (recursive variants),
      but are not referenced anywhere else.
    * any function that is declared but not called (including recursive functions).
      This rule should actually become somewhat more complex in the future:
      - mutually recursive functions that are not used anywhere will
        not be removed because they call each other
      - in a per-module compilation mode that is not implemented yet
        only "static", i.e. module-local functions should be removed.
        other unused functions should be excluded at the link stage.
    * if there is a value that is assigned but not used, we remove it as well.
      if it's not a temporary value, we issue a warning about unused value.
    * `if (true) a else b` is replaced with `a`
    * `if (false) a else b` is replaced with `b`
       if there is value/variable of type `void` that is assigned,
      `val a: void = f(...)`, it's replaced with `f(...)`
*)
