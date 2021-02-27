/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Sys

fun dir_sep() = if Sys.win32 {"\\"} else {"/"}

fun is_absolute(path: string) =
    path.startswith(dir_sep()) ||
    path.startswith("/") ||
    path.find(":") >= 0

fun is_relative(path: string) = !is_absolute(path)

fun split(path: string) {
    val sep = dir_sep()
    val sep0 = "/"
    assert(sep.length() == 1)
    if path.endswith(sep) || (sep != sep0 && path.endswith(sep0)) {
        if path.length() == 1 { (sep, sep) }
        else { split(path[:.-1]) }
    } else {
        val pos = path.rfind(sep)
        val pos0 = if sep == sep0 { pos } else { path.rfind(sep0) }
        val pos = if pos < 0 {pos0} else if pos0 < 0 {pos} else {max(pos, pos0)}
        if pos < 0 {
            (".", path)
        } else if pos == 0 || path[pos-1] == ':' {
            (path[:pos+1], path[pos+1:])
        } else {
            (path[:pos], path[pos+1:])
        }
    }
}

fun dirname(path: string) = split(path).0
fun basename(path: string) = split(path).1

fun concat(dir: string, fname: string) {
    val sep = dir_sep()
    val sep0 = "/"
    if dir.endswith(sep) || dir.endswith(sep0) {
        dir + fname
    } else {
        dir + sep + fname
    }
}

fun normalize(dir: string, fname: string)
{
    val sep = dir_sep()
    val sep0 = "/"
    assert(sep.length() == 1)
    if is_absolute(fname) {
        fname
    } else if fname.startswith("." + sep) || (sep != sep0 && fname.startswith("./")) {
        normalize(dir, fname[2:])
    } else if fname.startswith(".." + sep) || (sep != sep0 && fname.startswith("../")) {
        normalize(dirname(dir), fname[3:])
    } else {
        concat(dir, fname)
    }
}

fun remove_extension(path: string) {
    val sep = dir_sep()
    val sep0 = "/"
    val dotpos = path.rfind(".")
    if dotpos < 0 {
        path
    } else {
        val pos = path.rfind(sep)
        val pos0 = if sep == sep0 { pos } else { path.rfind(sep0) }
        val pos = if pos < 0 {pos0} else if pos0 < 0 {pos} else {max(pos, pos0)}
        if dotpos <= pos+1 { path }
        else { path[:dotpos]}
    }
}
