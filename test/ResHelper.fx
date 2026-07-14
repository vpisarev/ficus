/*
    Helper module for test/test_resolve.fx (WP-E D3). Kept separate so the test
    can exercise CROSS-MODULE resolution and the module-qualified operator-call
    escape hatch (Q4). Name is case-insensitively unique vs the stdlib.
*/
type rvec_t = {x: int; y: int}
operator * (a: rvec_t, b: rvec_t): int = a.x*b.x + a.y*b.y
// a generic membership using ==, to probe that an instance sees a caller's
// == overload (E0 / notes-#8) across a module boundary.
fun rmem[T](x: 't, l: list[T]): bool = exists(for y <- l {y == x})
