// expect: cannot use 'break' outside of loop
// ctrlflow-1: 'break' in statement position but not inside any loop. The
// diagnostic must name the construct, never the old 'unexpected token'.
fun f(): void { break }
f()
