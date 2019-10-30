/* math functions on scalars and arrays */

ccode "#include <math.h>"

val Pi = 3.1415926535897932384626433832795

pure nothrow fun pow(x: float, y: float): float = ccode "return powf(x, y);"
pure nothrow fun pow(x: double, y: double): double = ccode "return pow(x, y);"
pure nothrow fun sqrt(x: float): float = ccode "return sqrtf(x);"
pure nothrow fun sqrt(x: double): double = ccode "return sqrt(x);"

pure nothrow fun atan(x: float): float = ccode "return atanf(x);"
pure nothrow fun atan(x: double): double = ccode "return atan(x);"
pure nothrow fun atan2(y: float, x: float): float = ccode "return atan2f(x);"
pure nothrow fun atan2(y: double, x: double): double = ccode "return atan2(x);"
pure nothrow fun cos(x: float): float = ccode "return cosf(x);"
pure nothrow fun cos(x: double): double = ccode "return cos(x);"
pure nothrow fun sin(x: float): float = ccode "return sinf(x);"
pure nothrow fun sin(x: double): double = ccode "return sin(x);"
pure nothrow fun tan(x: float): float = ccode "return tanf(x);"
pure nothrow fun tan(x: double): double = ccode "return tan(x);"

pure nothrow fun log(x: float): float = ccode "return logf(x);"
pure nothrow fun log(x: double): double = ccode "return log(x);"
pure nothrow fun exp(x: float): float = ccode "return expf(x);"
pure nothrow fun exp(x: double): double = ccode "return exp(x);"

pure nothrow fun atanh(x: float): float = ccode "return atanhf(x);"
pure nothrow fun atanh(x: double): double = ccode "return atanh(x);"
pure nothrow fun cosh(x: float): float = ccode "return coshf(x);"
pure nothrow fun cosh(x: double): double = ccode "return cosh(x);"
pure nothrow fun sinh(x: float): float = ccode "return sinhf(x);"
pure nothrow fun sinh(x: double): double = ccode "return sinh(x);"
pure nothrow fun tanh(x: float): float = ccode "return tanhf(x);"
pure nothrow fun tanh(x: double): double = ccode "return tanh(x);"
