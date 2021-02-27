import Sys
import UTest
import test_basic
import test_nbody
import test_btree
import test_ycomb
import test_json
import test_spectralnorm
import test_mandelbrot
import test_closure
import test_re2
import test_ds
import test_filename

fun print_hdr()
{
    println(f"\33[35;1mFicus version:\33[0m {__ficus_version_str__} (git commit: {__ficus_git_commit__})")
    println(f"\33[35;1mPlaform:\33[0m {Sys.osname(true)}")
    println(f"\33[35;1mC/C++ Compiler:\33[0m {Sys.cc_version()}")
}

val (run, options) = UTest.test_parse_options(Sys.arguments(), "Ficus unit tests.", "")
if run {
    print_hdr()
    UTest.test_run_all(options)
}
