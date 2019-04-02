/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtin import *" directive in the beginning */

fun string(a: int): void = ccode "char buf[32]; sprintf(buf, \"%d\", a); return fx_make_cstring(fx_res, buf);"
fun string(a: float): void = ccode "char buf[32]; sprintf(buf, \"%.10g\", a); return fx_make_cstring(fx_res, buf);"
fun string(a: double): void = ccode "char buf[32]; sprintf(buf, \"%.20g\", a); return fx_make_cstring(fx_res, buf);"
fun string(a: string) = a
fun print_string(a: string): void = ccode "fx_puts(a.data); return FX_OK;"

fun print(a: 't) = print_string(string(a))
fun print(a: string) = print_string(a)

fun println(a: 't) { print(a); print("\n"); }
