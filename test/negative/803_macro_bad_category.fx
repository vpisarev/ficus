// expect: unknown macro parameter category '@blah'
// macro-1: macro parameters use the @expr (or @for_expr) categories only.
macro m(x: @blah): int = x
println(m(3))
