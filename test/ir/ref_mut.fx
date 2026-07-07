// IR snapshot: ref cell create / deref / assign
val r = ref(0)
*r = 5
*r += 3
println(*r)
