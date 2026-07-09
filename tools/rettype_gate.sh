#!/bin/bash
# annotate-2 WP-3 gate: every module-level function in the ANNOTATED stdlib
# scope must carry an explicit return type. Enforced via -Wimplicit-rettype
# promoted to an error with -Werror. Compiling each file with -no-c (parse +
# typecheck only) is cheap (~a few seconds for the whole scope).
#
# Why a compiler-based gate (not tools/lint_op_returns.py --funs): the textual
# linter cannot see function nesting, so it false-positives on nested generic
# helpers (e.g. Map.mem_); -Wimplicit-rettype is the typechecker's own verdict
# and is authoritative. The operator lint (lint_op_returns.py lib) stays as a
# separate, faster guard for the 106 generic operators.
#
# Scope EXCLUDES the not-yet-annotated application libraries: NN/, Onnx/,
# Protobuf/, and OpenCV.fx (see docs/annotate2_report.md). Everything else in
# lib/ -- including the small DSP/Drawing/Image modules -- must stay clean.
set -u
FICUS="${1:-bin/ficus}"

files=$(ls lib/*.fx | grep -v '/OpenCV\.fx$')
files="$files lib/DSP/Fft.fx lib/Drawing/Shapes.fx lib/Image/Decoder.fx lib/Image/Encoder.fx"

fail=0
for f in $files; do
    if ! "$FICUS" -no-c -Wall -Werror "$f" >/dev/null 2>&1; then
        echo "FAIL: unannotated module-level function(s) in $f"
        "$FICUS" -no-c -Wall "$f" 2>&1 | grep "implicit return type" | head
        fail=1
    fi
done

if [ $fail -eq 0 ]; then
    echo "rettype gate: clean (annotated stdlib scope, -Wimplicit-rettype -Werror)"
fi
exit $fail
