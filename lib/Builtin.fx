/* Ficus built-in module, i.e. each Ficus module is compiled
   as if it has "from Builtin import *" directive in the beginning */
type 't option = None | Some: 't

fun getOpt(x: 't option, defval: 't) = match (x) { | Some(x) => x | None => defval }

pure nothrow fun length(s: string): int = ccode "return s->length;"
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

fun atoi(a: string): int option
{
    fun atoi_(a: string): (int, bool) = ccode
        "return __fx_atoi(fx_ctx, a, &fx_result->v1, &fx_result->v2, 10);"
    match (atoi_(a)) {
    | (x, true) => Some(x)
    | _ => None
    }
}

fun print(a: 't) = print_string(string(a))
fun print(a: string) = print_string(a)

fun println() = print("\n")
fun println(a: 't) { print(a); print("\n") }

pure nothrow fun size(a: 't []): int = ccode "return a->size[0];"
