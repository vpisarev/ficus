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

val (run, options) = UTest.test_parse_options(Sys.arguments(), "Ficus unit tests.", "")
if run {UTest.test_run_all(options)}
