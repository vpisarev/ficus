/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Sys
import UTest
import test_basic
import test_array
import test_nbody
import test_btree
import test_ycomb
import test_json
import test_spectralnorm
import test_mandelbrot
import test_closure
import test_re
//import test_re2
import test_ds
import test_deque
import test_filename
import test_oop
import test_parallel
import test_vec

fun print_hdr()
{
    val (Color, Normal) = if Sys.colorterm() {("\33[35;1m", "\33[0m")} else {("", "")}
    println(f"{Color}Ficus version:{Normal} {__ficus_version_str__} (git commit: {__ficus_git_commit__})")
    println(f"{Color}Plaform:{Normal} {Sys.osname(true)}")
    println(f"{Color}C/C++ Compiler:{Normal} {Sys.cc_version()}")
}

val (run, options) = UTest.test_parse_options(Sys.arguments(), "Ficus unit tests.", "")
if run {
    print_hdr()
    UTest.test_run_all(options)
}
