/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtin import *" directive in the beginning */

fun string(a: int): string = ccode "char buf[32]; sprintf(buf, \"%d\", a); return __fx_make_cstring(fx_ctx, &fx_res, buf);"
fun string(a: float): string = ccode "char buf[32]; sprintf(buf, \"%.10g\", a); return __fx_make_cstring(fx_ctx, &fx_res, buf);"
fun string(a: double): string = ccode "char buf[32]; sprintf(buf, \"%.20g\", a); return __fx_make_cstring(fx_ctx, &fx_res, buf);"
fun string(a: string) = a
operator + (a: string, b: string): string = ccode "fx_string* s[] = {a, b}; return __fx_str_join(fx_ctx, &fx_res, 0, s, 2);"
operator + (a: string, b: char): string = ccode "
    fx_string bstr; fx_string* s[] = {a, &bstr};
    FX_STATUS fx_status = __fx_make_static_string(fx_ctx, &bstr, &b, 1);
    if(fx_status < 0)
        return fx_status;
    return fx_str_join(fx_ctx, fx_res, 0, s, 2);"
operator + (a: char, b: string): string = ccode "
    fx_string astr; fx_string* s[] = {&astr, b};
    FX_STATUS fx_status = __fx_make_static_string(fx_ctx, &astr, &a, 1);
    if(fx_status < 0)
        return fx_status;
    return fx_str_join(fx_ctx, fx_res, 0, s, 2);"

fun print_string(a: string): void = ccode "return __fx_puts(fx_ctx, a->data);"

fun print(a: 't) = print_string(string(a))
// [TODO] this overloaded variant can safely be removed later,
// but currently it's used to test custom overloading of a generic function
fun print(a: string) = print_string(a)

fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }
