import Args
import UTest
import test_basic
import test_nbody
import test_btree
import test_ycomb
import test_json
import test_spectralnorm
import test_closure
//import test_re2

val (run, options) = UTest.test_parse_options(Args.arguments())
if run {UTest.test_run_all(options)}
