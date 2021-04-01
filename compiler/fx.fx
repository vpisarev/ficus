/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Options, Compiler

val ok = Options.parse_options()
val ok = ok && Compiler.process_all(Options.opt.filename)
if !ok {throw Exit(1)}
