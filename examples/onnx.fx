/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// dumps .onnx model

import Onnx.Ast, Onnx.Parse, Sys
val model_name = match Sys.arguments() {
    | f :: [] => f
    | _ => println("specify .onnx model name"); throw Fail("")
    }
println(Onnx.Parse.parse(model_name))
