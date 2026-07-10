// expect: is not found
// reform-prep-1: an import of a missing/misspelled module underlines the whole
// (possibly dotted) module name, with a caret.
import Foo.Bar.Baz
println("hi")
