// expect: '@parallel' loop
// diag-1: 'break'/'continue' inside a @parallel loop is illegal. The check was
// lifted from C generation to the type checker, so it is now visible under
// -no-c, carries an exact caret, and can be locked by a golden.
val a = array(10, 0)
@parallel for i <- 0:10 { if i % 2 == 0 { continue } else {}; a[i] = i }
println(a)
