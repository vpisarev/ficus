// expect: replacing strided elements with a non-empty vector is not supported
val v = vector([0, 1, 2, 3, 4, 5])
v[::2] = vector([9, 9, 9])
