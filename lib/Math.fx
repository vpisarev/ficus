/* math functions on scalars and arrays */

ccode "#include <math.h>"

fun sqrt(x: float): float = ccode "*fx_res = sqrtf(x); return FX_OK;"
fun sqrt(x: double): double = ccode "*fx_res = sqrt(x); return FX_OK;"
fun pow(x: float, y: float): float = ccode "*fx_res = powf(x, y); return FX_OK;"
fun pow(x: double, y: double): double = ccode "*fx_res = pow(x, y); return FX_OK;"

fun sin(x: float): float = ccode "*fx_res = sinf(x); return FX_OK;"
fun sin(x: double): double = ccode "*fx_res = sin(x); return FX_OK;"
fun cos(x: float): float = ccode "*fx_res = cosf(x); return FX_OK;"
fun cos(x: double): double = ccode "*fx_res = cos(x); return FX_OK;"
fun tan(x: float): float = ccode "*fx_res = tanf(x); return FX_OK;"
fun tan(x: double): double = ccode "*fx_res = tan(x); return FX_OK;"
fun atan(x: float): float = ccode "*fx_res = atanf(x); return FX_OK;"
fun atan(x: double): double = ccode "*fx_res = atan(x); return FX_OK;"
fun atan2(y: float, x: float): float = ccode "*fx_res = atan2f(x); return FX_OK;"
fun atan2(y: double, x: double): double = ccode "*fx_res = atan2(x); return FX_OK;"

fun exp(x: float): float = ccode "*fx_res = expf(x); return FX_OK;"
fun exp(x: double): double = ccode "*fx_res = exp(x); return FX_OK;"
fun log(x: float): float = ccode "*fx_res = logf(x); return FX_OK;"
fun log(x: double): double = ccode "*fx_res = log(x); return FX_OK;"

fun sinh(x: float): float = ccode "*fx_res = sinhf(x); return FX_OK;"
fun sinh(x: double): double = ccode "*fx_res = sinh(x); return FX_OK;"
fun cosh(x: float): float = ccode "*fx_res = coshf(x); return FX_OK;"
fun cosh(x: double): double = ccode "*fx_res = cosh(x); return FX_OK;"
fun tanh(x: float): float = ccode "*fx_res = tanhf(x); return FX_OK;"
fun tanh(x: double): double = ccode "*fx_res = tanh(x); return FX_OK;"
fun atanh(x: float): float = ccode "*fx_res = atanhf(x); return FX_OK;"
fun atanh(x: double): double = ccode "*fx_res = atanh(x); return FX_OK;"
