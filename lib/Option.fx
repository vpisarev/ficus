/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// 't?, int? etc. can be used instead of 't option, int option ...
module type 't option = None | Some: 't

fun some_or(x: 't?, defval: 't) = match x { | Some(x) => x | _ => defval }
fun getsome(x: 't?) = match x { | Some(x) => x | _ => throw OptionError }
fun isnone(x: 't?) { | Some _ => false | _ => true }
fun issome(x: 't?) { | Some _ => true | _ => false }

pure nothrow operator == (a: 't?, b: 't?) {
    | (Some(a), Some(b)) => a == b
    | (None, None) => true
    | _ => false
}
pure nothrow operator <=> (a: 't?, b: 't?) {
    | (Some(a), Some(b)) => a <=> b
    | (None, Some _) => -1
    | (Some _, None) => 1
    | _ => 0
}

fun string(a: 't?) {
    | Some(a) => "Some(" + repr(a) + ")"
    | _ => "None"
}
