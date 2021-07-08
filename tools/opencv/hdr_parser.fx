/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    This is a conversion of opencv's hdr_parser.py script that parses OpenCV headers and
    extracts information about API, sufficient to generate bindings for various languages.
    Here is the original OpenCV license: https://github.com/opencv/opencv/blob/master/LICENSE
*/

import Sys, Re, File, Filename, Hashset, Hashmap

// the list only for debugging. The real list, used in the real OpenCV build, is specified in CMakeLists.txt
val opencv_hdr_list = [
    "core/include/opencv2/core.hpp",
    "core/include/opencv2/core/mat.hpp",
    "core/include/opencv2/core/ocl.hpp",
    "flann/include/opencv2/flann/miniflann.hpp",
    "ml/include/opencv2/ml.hpp",
    "imgproc/include/opencv2/imgproc.hpp",
    "3d/include/opencv2/3d.hpp",
    "stereo/include/opencv2/stereo.hpp",
    "calib/include/opencv2/calib.hpp",
    "features2d/include/opencv2/features2d.hpp",
    "video/include/opencv2/video/tracking.hpp",
    "video/include/opencv2/video/background_segm.hpp",
    "objdetect/include/opencv2/objdetect.hpp",
    "imgcodecs/include/opencv2/imgcodecs.hpp",
    "videoio/include/opencv2/videoio.hpp",
    "highgui/include/opencv2/highgui.hpp",
    "dnn/include/opencv2/dnn/dnn.hpp",
]

type block_t = BlockNone | BlockGeneric | BlockEnumStruct | BlockEnumClass | BlockEnum | BlockFile | BlockStruct | BlockClass | BlockNamespace
type state_t = StateScan | StateComment | StateDirective | StateDocstring | StateDirectiveIf0

/*
Each declaration is [funcname, return_value_type (in C, not in Python), <list_of_modifiers>, <list_of_arguments>, original_return_type, docstring],
where each element of <list_of_arguments> is 4-element list itself:
[argtype, argname, default_value (or "" if none), <list_of_modifiers>]
where the list of modifiers is yet another nested list of strings
   (currently recognized are "/O" for output argument, "/S" for static (i.e. class) methods
   and "/A value" for the plain C arrays with counters)
original_return_type is None if the original_return_type is the same as return_value_type
*/
type arg_info_t =
{
    name: string
    arg_type: string
    defval: string=""
    readwrite: bool=false
    isinout: bool=false
    isoutput: bool=false
    isconst: bool=false
    isref: bool=false
    isrref: bool=false
    isarray: string?
    custom_array: string?
}

type decl_t =
    | DeclFunc : {
        name: string
        rettype: string=""
        args: arg_info_t list
        orig_rettype: string=""
        docstring: string=""
        name_alias: string=""
        phantom: bool=false
        mappable: string=""
        static_method: bool=false
        const_method: bool=false
        virtual_method: bool=false
        pure_virtual_method: bool=false
    }
    | DeclEnum : {
        name: string
        elems: arg_info_t list
    }
    | DeclClass : {
        name: string
        bases: string list
        name_alias: string
        docstring: string=""
        members: arg_info_t list ref
        ismap: bool = false
        isparams: bool = false
        issimple: bool = false
    }

fun string(s: state_t) {
    | StateScan => "StateScan"
    | StateComment => "StateComment"
    | StateDirective => "StateDirective"
    | StateDocstring => "StateDocstring"
    | StateDirectiveIf0 => "StateDirectiveIf0"
}

fun string(b: block_t) {
    | BlockEnumStruct => "BlockEnumStruct"
    | BlockEnumClass => "BlockEnumClass"
    | BlockEnum => "BlockEnum"
    | BlockFile => "BlockFile"
    | BlockStruct => "BlockStruct"
    | BlockClass => "BlockClass"
    | BlockNamespace => "BlockNamespace"
    | BlockGeneric => "BlockGeneric"
    | BlockNone => "BlockNone"
}

type block_info_t
{
    block_type: block_t
    block_name: string
    process: bool
    public_section: bool
    decl: decl_t?
}

class hdr_parser_t
{
    var block_stack: block_info_t list
    gen_gpumat_decls: bool
    gen_umat_decls: bool
    hname: string
    var lineno: int
    var namespaces: string Hashset.t
    wrap_mode: bool
}

fun parse_err(self: hdr_parser_t, msg: string)
{
    println(f"{self.hname}:{self.lineno}: error: {msg}")
    Exit(-1)
}

fun batch_replace(s0: string, pairs: (string, string) list) =
    fold s=s0 for (f, t) <- pairs {s.replace(f, t)}

/*
Finds the next token from the 'tlist' in the input 's', starting from position 'p'.
Returns the first occurred token and its position, or ("", len(s)) when no token is found
*/
fun hdr_parser_t.find_next_token(s: string, tlist: string list, pos0: int): (string, int) =
    fold token="", tpos=length(s) for t <- tlist {
        val pos = s.find(t, pos0)
        if 0 <= pos < tpos {(t, pos)} else {(token, tpos)}
    }

/*
    The main method. Parses the input file.
    Returns the list of declarations (that can be print using print_decls)
*/
fun parse(hname: string, ~wrap_mode:bool=true)
{
    val self = hdr_parser_t
    {
        block_stack = block_info_t {
            block_type=BlockFile,
            block_name=hname,
            process=true,
            public_section=true,
            decl=None
        } :: [],
        gen_gpumat_decls=false,
        gen_umat_decls=false,
        hname=hname,
        lineno=0,
        namespaces=Hashset.empty(16, ""),
        wrap_mode=wrap_mode
    }

    var decls: decl_t list = []
    val f = File.open(hname, "rt")
    var linelist = []
    while !f.eof() {
        linelist = f.readln() :: linelist
    }
    linelist = linelist.rev()
    f.close()

    var state = StateScan

    var block_head = ""
    var docstring = ""
    var depth_if_0 = 0

    val re_eol_comment = Re.compile(r"//(.+)?")
    val re_eq_empty_block = Re.compile(r"=\s*\{\s*\}")

    for l0 <- linelist {
        //println(f"{self.lineno}. {state}. parsing '{l0.rstrip()}'")
        //println(f"block_head='{block_head}'")
        self.lineno += 1

        val l = l0.strip()

        //G-API specific aliases
        var l = batch_replace(l, [
                ("GAPI_EXPORTS", "CV_EXPORTS"),
                ("GAPI_EXPORTS_W", "CV_EXPORTS_W"),
                ("GAPI_EXPORTS_W_SIMPLE","CV_EXPORTS_W_SIMPLE"),
                ("GAPI_WRAP", "CV_WRAP"),
                ("GAPI_PROP", "CV_PROP"),
                ("defined(GAPI_STANDALONE)", "0"),
            ])

        if state == StateScan && l.startswith("#") {
            state = StateDirective
        }
        //fall through to the if state == DIRECTIVE check

        if state == StateDirective {
            if l.endswith("\\") {
                continue
            }
            state = StateScan
            l = re_eol_comment.replace(l, "").strip()  //drop // comment
            match l {
            | "#if 0" | "#if defined(__OPENCV_BUILD)" | "#ifdef __OPENCV_BUILD"
            | "#if !defined(OPENCV_BINDING_PARSER)" | "#ifndef OPENCV_BINDING_PARSER" =>
                state = StateDirectiveIf0
                depth_if_0 = 1
            | _ => {}
            }
            continue
        }

        if state == StateDirectiveIf0 {
            if l.startswith('#') {
                l = l[1:].strip()
                if l.startswith("if") {
                    depth_if_0 += 1
                    continue
                }
                if l.startswith("endif") {
                    depth_if_0 -= 1
                    if depth_if_0 == 0 {
                        state = StateScan
                    }
                }
            }
            continue
        }

        if state == StateComment {
            val pos = l.find("*/")
            if pos < 0 { continue }
            l = l[pos+2:]
            state = StateScan
        }

        if state == StateDocstring {
            val pos = l.find("*/")
            if pos < 0 {
                docstring += l0
                continue
            }
            docstring += l[:pos] + "\n"
            l = l[pos+2:]
            state = StateScan
        }

        if l.startswith("CV__") || l.startswith("__CV_") { //just ignore these lines
            //print(f"IGNORE: {l}")
            state = StateScan
            continue
        }

        if state != StateScan {
            throw self.parse_err(f"invalid state = {state}")
        }

        while true {
            //NB: Avoid parsing '{' for case:
            //foo(Obj&& = {});
            var (token, pos) = match re_eq_empty_block.find(l) {
                | Some(_) => (";", l.length())
                | _ => self.find_next_token(l, [";", "\"", "{", "}", "//", "/*"], 0)
            }

            if token == "" {
                block_head += " " + l
                block_head = block_head.strip()
                if block_head != "" && block_head.endswith(')') && block_head.startswith("CV_ENUM_FLAGS(") {
                    l = ""
                    token = ";"
                    pos = 0
                } else {
                    break
                }
            }

            if token == "//" {
                block_head += " " + l[:pos]
                l = ""
                continue
            }

            if token == "/*" {
                block_head += " " + l[:pos]
                val end_pos = l.find("*/", pos+2)
                if l.length() > pos + 2 && l[pos+2] == '*' {
                    // "/**", it's a docstring
                    if end_pos < 0 {
                        state = StateDocstring
                        docstring = l[pos+3:] + "\n"
                        break
                    }
                    else {
                        docstring = l[pos+3:end_pos]
                    }
                }
                else if end_pos < 0 {
                    state = StateComment
                    break
                }
                l = l[end_pos+2:]
                continue
            }

            if token == "\"" {
                var pos2 = pos + 1
                while true {
                    val (t2, pos2_) = self.find_next_token(l, ["\\", "\""], pos2)
                    if t2 == "" {
                        throw self.parse_err("no terminating '\"'")
                    } else if t2 == "\"" {
                        pos2 = pos2_
                        break
                    }
                }
                block_head += " " + l[:pos2+1]
                l = l[pos2+1:]
                continue
            }

            val stmt = (block_head + " " + l[:pos]).strip()
            val stmt = " ".join(stmt.split(' ', allow_empty=false)) //normalize the statement
            //print(stmt)
            val stack_top = self.block_stack.hd()

            if stmt.startswith("@") {
                //Objective C ?
                break
            }

            val (block_type, name, parse_flag, decl_opt) =
            if stack_top.process {
                //even if stack_top.public_section is false, we still try to process the statement,
                //since it can start with "public:"
                docstring = docstring.strip()
                val (block_type, name, parse_flag, decl_opt) = self.parse_stmt(stmt, token, docstring=docstring)
                match decl_opt {
                | Some(decl) =>
                    decls = decl :: decls
                    docstring = ""
                | _ => {}
                }
                if block_type == BlockNamespace {
                    val fold nested = [] for b <- self.block_stack {
                        | {block_type=BlockNamespace, block_name} => block_name :: nested
                        | _ => nested
                        }
                    self.namespaces.add(".".join((name :: nested).rev()))
                }
                (block_type, name, parse_flag, decl_opt)
            }
            else {
                (BlockGeneric, "", false, None)
            }

            if token == "{" {
                val public_section = block_type != BlockClass
                self.block_stack =
                    block_info_t {
                        block_type=block_type,
                        block_name=name,
                        process=parse_flag,
                        public_section=public_section,
                        decl=decl_opt
                    } :: self.block_stack
            } else if token == "}" {
                match self.block_stack {
                | _ :: rest =>
                    self.block_stack = rest
                    if pos+1 < l.length() && l[pos+1] == ';' {
                        pos += 1
                    }
                | _ =>
                    throw self.parse_err("block stack is empty")
                }
            }

            block_head = ""
            l = if pos >= l.length() {""} else {l[pos+1:]}
        }
    }

    (self.namespaces, decls.rev())
}

fun hdr_parser_t.get_macro_arg(arg_str: string, npos: int)
{
    val npos2 = arg_str.find("(", npos)
    var npos3 = npos2
    if npos2 < 0 {
        throw self.parse_err("no arguments for the macro")
    }
    var balance = 1
    while true {
        val (t, npos3_) = self.find_next_token(arg_str, ["(", ")"], npos3+1)
        npos3 = npos3_
        if npos3 < 0 {
            throw self.parse_err("no matching ')' in the macro call")
        }
        if t == "(" {
            balance += 1
        }
        if t == ")" {
            balance -= 1
            if balance == 0 {
                break
            }
        }
    }
    (arg_str[npos2+1:npos3].strip(), npos3)
}

/*
    Parses <arg_type> [arg_name]
    Returns arg_type, arg_name, modlist, argno, where
    modlist is the list of wrapper-related modifiers (such as "output argument", "has counter", ...)
    and argno is the new index of an anonymous argument.
    That is, if no arg_str is just an argument type without argument name, the argument name is set to
    "arg" + str(argno), and then argno is incremented.
*/
fun hdr_parser_t.parse_arg(arg_str0: string, argno: int)
{
    //println(f"parse arg #{argno} '{arg_str0}'")
    var arg_str = arg_str0
    var argno = argno
    var isinout = false, isoutput = false

    //pass 0: extracts the modifiers
    if arg_str.contains("CV_OUT") {
        isoutput = true
        arg_str = arg_str.replace("CV_OUT", "")
    }

    if arg_str.contains("CV_IN_OUT") {
        isinout = true
        arg_str = arg_str.replace("CV_IN_OUT", "")
    }

    var isarray = None
    var npos = arg_str.find("CV_CARRAY")
    if npos >= 0 {
        val (macro_arg, npos3) = self.get_macro_arg(arg_str, npos)
        isarray = Some(macro_arg)
        arg_str = arg_str[:npos] + arg_str[npos3+1:]
    }

    var custom_array = None
    npos = arg_str.find("CV_CUSTOM_CARRAY")
    if npos >= 0 {
        val (macro_arg, npos3) = self.get_macro_arg(arg_str, npos)
        custom_array = Some(macro_arg)
        arg_str = arg_str[:npos] + arg_str[npos3+1:]
    }

    val isconst = arg_str.contains("const")

    val isrref = arg_str.contains("&&")
    if isrref {
        arg_str = arg_str.replace("&&", "")
    }

    val isref = arg_str.contains("&")

    arg_str = arg_str.strip()
    var word_start = 0
    var word_list = []
    npos = -1

    // print self.lineno, ":\t", arg_str

    //pass 1: split argument type into tokens
    while true {
        npos += 1
        val (t, npos_) = self.find_next_token(arg_str, [" ", "&", "*", "<", ">", ","], npos)
        npos = npos_
        val w = if npos >= 0 {arg_str[word_start:npos].strip()} else {arg_str[word_start:].strip()}
        if w == "operator" {
            word_list = ("operator " + arg_str[npos:].strip()) :: word_list
            break
        }
        if w != "" && w != "const" {
            word_list = w :: word_list
        }
        if t != "" && t != " " && t != "&" {
            word_list = t :: word_list
        }
        if t == "" {
            break
        }
        word_start = npos+1
    }

    word_list = word_list.rev()
    //println(f"word_list: {word_list}")
    var arg_type = ""
    var arg_name = ""
    var angle_stack = []

    //print self.lineno, ":\t", word_list

    //pass 2: decrypt the list
    var wi = -1
    var prev_w = ""
    for w <- word_list {
        wi += 1
        if w == "*" {
            if prev_w == "char" && isarray.isnone() {
                arg_type = arg_type[:-length("char")] + "c_string"
            }
            else {
                arg_type += w
            }
            continue
        } else if w == "<" {
            arg_type += "_"
            angle_stack = 0 :: angle_stack
        } else if w == "," || w == ">" {
            if angle_stack == [] {
                throw self.parse_err("argument contains ',' or '>' not within template arguments")
            }
            if w == "," {
                arg_type += "_and_"
            } else if w == ">" {
                angle_stack = match angle_stack {
                | 0 :: _ =>
                    throw self.parse_err("template has no arguments")
                | _ :: rest =>
                    arg_type += "_end_"
                    rest
                }
            }
        } else if angle_stack != [] {
            arg_type += w
            angle_stack = (angle_stack.hd() + 1) :: angle_stack.tl()
        } else if arg_type == "struct" {
            arg_type += " " + w
        } else if arg_type != "" && arg_type != "~" {
            arg_name = " ".join(word_list.skip(wi))
            break
        } else {
            arg_type += w
        }
        prev_w = w
    }

    var counter_str = ""
    var add_star = false
    if arg_name.contains("[") && !arg_str.contains("operator") {
        //print arg_str
        val p1 = arg_name.find("[")
        val p2 = arg_name.find("]", p1+1)
        if p2 < 0 {
            throw self.parse_err("no closing ]")
        }
        counter_str = arg_name[p1+1:p2].strip()
        if counter_str == "" {
            counter_str = "?"
        }
        if isarray.isnone() {
            isarray = Some(counter_str.strip())
        }
        arg_name = arg_name[:p1]
        add_star = true
    }

    if arg_name == "" {
        if arg_type.startswith("operator") {
            arg_name = arg_type
            arg_type = ""
        } else {
            arg_name = "arg" + string(argno)
            argno += 1
        }
    }

    while arg_type.endswith("_end_") {
        arg_type = arg_type[:.-length("_end_")]
    }

    if add_star {
        arg_type += "*"
    }

    arg_type = batch_replace(arg_type, [("std::", ""), ("cv::", ""), ("::", "_")])
    //println(f"arg_type='{arg_type}'")

    (arg_info_t {
        name=arg_name,
        arg_type=arg_type,
        defval="",
        isinout=isinout,
        isoutput=isoutput,
        isconst=isconst,
        isref=isref,
        isrref=isrref,
        isarray=isarray,
        custom_array=custom_array
    }, argno)
}

fun hdr_parser_t.parse_enum(decl_str: string)
{
    val l = decl_str
    val ll = l.split(',', allow_empty=false)
    var prev_val = ""
    var prev_val_delta = -1
    var elems = []
    for pair <- ll {
        val p = pair.strip()
        if p == "" { continue }
        val (curr_name, curr_val) = match p.split('=', allow_empty=false) {
            | curr_name :: [] =>
                prev_val_delta += 1
                val curr_val = (if prev_val != "" {prev_val + "+"} else {""}) + string(prev_val_delta)
                (curr_name, curr_val)
            | curr_name :: curr_val :: [] =>
                prev_val_delta = 0
                val curr_val = curr_val.strip()
                prev_val = curr_val
                (curr_name, curr_val)
            | _ => throw self.parse_err("invalid enum value definition")
        }
        elems = arg_info_t {
            name=self.get_dotted_name(curr_name),
            arg_type="int",
            defval=curr_val,
            isarray=None,
            custom_array=None } :: elems
    }
    elems.rev()
}

/*
    Parses class/struct declaration start in the form:
        {class|struct} [CV_EXPORTS] <class_name> [: public <base_class1> [, ...]]
    Returns class_name1, <list of base_classes>
*/
fun hdr_parser_t.parse_class_decl(decl_str: string, ~docstring: string="")
{
    var l = decl_str
    var ismap = l.contains("CV_EXPORTS_W_MAP")
    if ismap {
        l = l.replace("CV_EXPORTS_W_MAP", "")
    }
    val isparams = l.contains("CV_EXPORTS_W_PARAMS")
    if isparams {
        ismap = true
        l = l.replace("CV_EXPORTS_W_PARAMS", "")
    }
    val issimple = l.contains("CV_EXPORTS_W_SIMPLE")
    if issimple {
        l = l.replace("CV_EXPORTS_W_SIMPLE", "")
    }
    var npos = l.find("CV_EXPORTS_AS")
    if npos < 0 {
        npos = l.find("CV_WRAP_AS")
    }

    val name_alias =
        if npos >= 0 {
            val (macro_arg, npos3) = self.get_macro_arg(l, npos)
            l = l[:npos] + l[npos3+1:]
            macro_arg
        } else {""}

    l = batch_replace(l, [("CV_EXPORTS_W", ""), ("CV_EXPORTS", ""), ("public virtual ", " "), ("public ", " "), ("::", ".")]).strip()
    npos = l.find(':')
    val (classname, bases) = if npos >= 0 {
        val classname = l[:npos].strip()
        (classname, [for b <- l[npos+1:].split(',', allow_empty=false) {b.strip()}])
    } else {
        (l.strip(), [])
    }
    val classname = classname.replace("class ", "").replace("struct ", "").strip()
    DeclClass {
        name=self.get_dotted_name(classname),
        bases=[for b <- bases {self.get_dotted_name(b)}],
        name_alias=name_alias,
        members=ref [],
        docstring=docstring,
        ismap=ismap,
        isparams=isparams,
        issimple=issimple
    }
}

val re_eq0 = Re.compile(r"=\s*0")
val re_ctor_dtor = Re.compile(r"(?:\w+::)*(\w+::)?~?(\w+)@")
val re_typedef_func = Re.compile(r"\w+\s+\(\*\w+\)\s*\(.*\)")
val re_typedef_method = Re.compile(r"\w+\s+\(\w+::\*\w+\)\s*\(.*\)")
val re_macro_name = Re.compile(r"[A-Z_][A-Z0-9_]*@")
val re_2darray = Re.compile(r"\w+\s+\(\*\w+\)\[\d+\]")

/*
    Parses the function or method declaration in the form:
    [([CV_EXPORTS] <rettype>) | CVAPI(rettype)]
        [~]<function_name>
        (<arg_type1> <arg_name1>[=<default_value1>] [, <arg_type2> <arg_name2>[=<default_value2>] ...])
        [const] {; | <function_body>}

    Returns the function declaration entry:
    [<func name>, <return value C-type>, <list of modifiers>, <list of arguments>, <original return type>, <docstring>] (see above)
*/
fun hdr_parser_t.parse_func_decl(decl_str: string, ~mat: string="Mat", ~docstring:string="")
{
    var decl_str = decl_str
    if !decl_str.contains("CV_EXPORTS_AS") && !decl_str.contains("CV_EXPORTS_W") && !decl_str.contains("CV_WRAP") {
        None
    } else if decl_str.contains("CVAPI(") {
        None
    } else {
        val top = self.block_stack.hd()
        var npos = decl_str.find("CV_EXPORTS_AS")
        var name_alias =
            if npos >= 0 {
                val (arg, npos3) = self.get_macro_arg(decl_str, npos)
                decl_str = decl_str[:npos] + decl_str[npos3+1:]
                arg
            } else {""}
        npos = decl_str.find("CV_WRAP_AS")
        if npos >= 0 {
            val (arg, npos3) = self.get_macro_arg(decl_str, npos)
            decl_str = decl_str[:npos] + decl_str[npos3+1:]
            name_alias = arg
        }
        npos = decl_str.find("CV_WRAP_PHANTOM")
        val phantom = if npos >= 0 {
                val (arg, _) = self.get_macro_arg(decl_str, npos)
                decl_str = arg
                true
            } else { false }
        /*npos = decl_str.find("CV_WRAP_MAPPABLE")
        val mappable = if npos >= 0 {
            val (mappable, npos3) = self.get_macro_arg(decl_str, npos)
            func_modlist.append("/mappable="+mappable)
        classname = top[1]
        return ['.'.join([classname, classname]), None, func_modlist, [], None, None]*/

        //filter off some common prefixes, which are meaningless for Python wrappers.
        //note that we do not strip "static" prefix, which does matter;
        //it means class methods, not instance methods
        decl_str = batch_replace(decl_str,
            [("static inline", ""), ("inline", ""), ("explicit ", ""),
            ("CV_EXPORTS_W", ""), ("CV_EXPORTS", ""), ("CV_CDECL", ""),
            ("CV_WRAP ", " "), ("CV_INLINE", ""), ("CV_DEPRECATED", ""),
            ("CV_DEPRECATED_EXTERNAL", "")]).strip()

        val virtual_method = decl_str.strip().startswith("virtual")
        if virtual_method {
            decl_str = decl_str.replace("virtual", "")
        }

        val npos2 = decl_str.rfind(')')
        val const_method = npos2 > 0 && decl_str[npos2+1:].contains("const")
        val pure_virtual_method = re_eq0.find(decl_str[npos2+1:]).issome()

        val static_method =
            decl_str.strip().startswith("static") &&
            (match top.block_type { | BlockStruct | BlockClass => true | _ => false })
        if static_method {
            decl_str = decl_str.strip()[length("static"):].lstrip()
        }

        var arg_start = decl_str.find("(")
        if decl_str.startswith("CVAPI") {
            val rtype_end = decl_str.find(")", arg_start+1)
            if rtype_end < 0 {
                throw self.parse_err("No terminating ')' in CVAPI() macro")
            }
            decl_str = decl_str[arg_start+1:rtype_end] + " " + decl_str[rtype_end+1:]
            arg_start = decl_str.find("(")
        }
        if arg_start < 0 {
            throw self.parse_err(f"No args in '{decl_str}'")
        }

        var decl_start = decl_str[:arg_start].strip()
        // handle operator () case
        if decl_start.endswith("operator") {
            arg_start = decl_str.find("(", arg_start+1)
            if arg_start < 0 {
                throw self.parse_err(f"No args in '{decl_str}'")
            }
            decl_start = decl_str[:arg_start].strip()
            //TODO: normalize all types of operators
            if decl_start.endswith("()") {
                decl_start = decl_start[:.-2].rstrip() + " ()"
            }
        }

        // constructor/destructor case
        decl_start = match re_ctor_dtor.prefixmatch_str(decl_start + "@") {
            | Some(r) when r[1] == "" || r[1] == r[2] => "void " + decl_start
            | _ => decl_start
            }

        val (fname_arg_info, argno) = self.parse_arg(decl_start, -1)
        val {name=funcname, arg_type=rettype} = fname_arg_info

        // determine original return type, hack for return types with underscore
        var orig_rettype = ""
        val i = decl_start.rfind(funcname)
        if i > 0 {
            orig_rettype = decl_start[:i].replace("&", "").replace("const", "").strip()
        }

        val (rettype, funcname) =
            if argno >= 0 {
                val classname = top.block_name
                if rettype == classname || rettype == "~" + classname {
                    ("", rettype)
                } else {
                    if re_typedef_func.fullmatch(decl_str) ||
                       re_typedef_method.fullmatch(decl_str) ||
                       re_macro_name.fullmatch(decl_start+"@") ||
                       re_2darray.fullmatch(decl_str) {
                           (rettype, "")
                    } else {
                        // print rettype, funcname, modlist, argno
                        throw self.parse_err(f"function/method name is missing: '{decl_start}'")
                    }
                }
            } else {(rettype, funcname)}
        if funcname == "" || funcname.contains("::") || funcname.startswith("~") {
            None
        } else {
            val funcname = self.get_dotted_name(funcname)
            npos = arg_start
            arg_start += 1
            var balance = 1
            var angle_balance = 0
            //scan the argument list; handle nested parentheses
            var args = []
            var argno = 1

            while balance > 0 {
                npos += 1
                val (t, npos_) = self.find_next_token(decl_str, ["(", ")", ",", "<", ">"], npos)
                npos = npos_

                if t == "" {
                    throw self.parse_err("no closing ')'")
                } else if t == "<" {
                    angle_balance += 1
                } else if t == ">" {
                    angle_balance -= 1
                } else if t == "(" {
                    balance += 1
                } else if t == ")" {
                    balance -= 1
                }
                if balance == 0 || (t == "," && balance == 1 && angle_balance == 0) {
                    //process next function argument
                    var a = decl_str[arg_start:npos].strip()
                    // print "arg = ", a
                    arg_start = npos+1
                    if a != "" {
                        var eqpos = a.find("=")
                        val defval = if eqpos >= 0 {a[eqpos+1:].strip()}
                            else {
                                eqpos = a.find("CV_DEFAULT")
                                if eqpos >= 0 {
                                    self.get_macro_arg(a, eqpos).0
                                } else {
                                    eqpos = a.find("CV_WRAP_DEFAULT")
                                    if eqpos >= 0 {
                                        self.get_macro_arg(a, eqpos).0
                                    } else {""}
                                }
                            }
                        val defval = if defval == "NULL" {"0"} else {defval}
                        if eqpos >= 0 {
                            a = a[:eqpos].strip()
                        }
                        val (arg_info, argno_) = self.parse_arg(a, argno)
                        argno = argno_
                        // TODO: Vectors could contain UMat, but this is not very easy to support and not very needed
                        val mat_arg = "Mat"
                        val vector_mat_arg = "vector_Mat"
                        val vector_mat_template = "vector<Mat>"
                        val arg_type = arg_info.arg_type
                        var isinout = arg_info.isinout, isoutput = arg_info.isoutput
                        val (arg_type, isinout, isoutput) = match arg_type {
                            | "InputArray" => (mat_arg, false, false)
                            | "InputOutputArray" => (mat_arg, true, false)
                            | "OutputArray" => (mat_arg, false, true)
                            | "InputArrayOfArrays" => (vector_mat_arg, false, false)
                            | "InputOutputArrayOfArrays" => (vector_mat_arg, true, false)
                            | "OutputArrayOfArrays" => (vector_mat_arg, false, true)
                            | _ => (arg_type, isinout, isoutput)
                            }
                        val defval = batch_replace(defval,
                            [("InputArrayOfArrays", vector_mat_template),
                            ("InputOutputArrayOfArrays", vector_mat_template),
                            ("OutputArrayOfArrays", vector_mat_template),
                            ("InputArray", mat_arg),
                            ("InputOutputArray", mat_arg),
                            ("OutputArray", mat_arg),
                            ("noArray", arg_type)]).strip()

                        args = arg_info.{arg_type=arg_type, defval=defval, isinout=isinout, isoutput=isoutput} :: args
                        npos = arg_start
                    }
                }
            }

            Some(DeclFunc {
                name=funcname,
                rettype=rettype,
                args=args.rev(),
                orig_rettype=orig_rettype,
                docstring=docstring,
                name_alias=name_alias,
                phantom=phantom,
                static_method=static_method,
                virtual_method=virtual_method,
                pure_virtual_method=pure_virtual_method,
                const_method=const_method
            })
        }
    }
}

/*
    adds the dot-separated container class/namespace names to the bare function/class name, e.g. when we have

    namespace cv {
    class A {
    public:
        f(int);
    };
    }

    the function will convert "A" to "cv.A" and "f" to "cv.A.f".
*/
fun hdr_parser_t.get_dotted_name(name: string) =
    if self.block_stack == [] || name.startswith("cv.") {
        name
    } else {
        val qualified_name = name.contains('.') || name.contains("::")
        var n = ""
        for b <- self.block_stack.rev() {
            val {block_type, block_name} = b
            match block_type {
            | BlockFile | BlockEnum => {}
            | BlockEnumClass | BlockEnumStruct when block_name == name => {}
            | BlockStruct | BlockClass | BlockNamespace | BlockEnumClass | BlockEnumStruct =>
                if block_name != "" && (block_type == BlockNamespace || !qualified_name) {
                    val bare_block_name = match block_type {
                        | BlockNamespace => block_name
                        | _ =>
                            val dotpos = block_name.rfind('.')
                            val bare_block_name = if dotpos >= 0 {block_name[dotpos+1:]} else {block_name}
                            bare_block_name.replace("class ", "").replace("struct ", "").strip()
                        }
                    n += bare_block_name + "."
                }
            | _ =>
                throw self.parse_err(f"unsupported block '{block_type}' in the block stack")
            }
        }
        n += name.replace("::", ".")
        if n.endswith(".Algorithm") {
            n = "cv.Algorithm"
        }
        n
    }

/*
parses the statement (ending with ';' or '}') or a block head (ending with '{')

The function calls parse_class_decl or parse_func_decl when necessary. It returns
<block_type>, <block_name>, <parse_flag>, <declaration>
where the first 3 values only make sense for blocks (i.e. code blocks, namespaces, classes, enums and such)
*/
fun hdr_parser_t.parse_stmt(stmt: string, end_token: string,
                            ~mat: string="Mat", ~docstring: string=""):
    (block_t, string, bool, decl_t?)
{
    //println(f"parsing stmt '{stmt}'")
    var stmt = stmt
    var stack_top = self.block_stack.hd()
    val context = stack_top.block_type

    if stmt.startswith("inline namespace") {
        //emulate anonymous namespace
        (BlockNamespace, "", true, None)
    } else {
        var stmt_type = BlockNone
        if end_token == "{" {
            stmt_type = BlockGeneric
        }
        if context == BlockGeneric {
            throw self.parse_err("should not call parse_stmt inside code blocks")
        }
        if context == BlockClass || context == BlockStruct {
            val colon_pos = stmt.find(":")
            if colon_pos > 0 {
                val w = stmt[:colon_pos].strip()
                if w == "public" || w == "protected" || w == "private" {
                    stack_top = stack_top.{public_section = w == "public"}
                    self.block_stack = stack_top :: self.block_stack.tl()
                    stmt = stmt[colon_pos+1:].strip()
                }
            }
        }
        if !stack_top.public_section || stmt.startswith("template") {
            (stmt_type, "", false, None)
        } else {
            if end_token == "{" && (stmt.startswith("class ") || stmt.startswith("struct ")) {
                stmt_type = if stmt.startswith("class") {BlockClass} else {BlockStruct}
                val class_decl = self.parse_class_decl(stmt, docstring=docstring)
                val classname = match class_decl {
                    | DeclClass {name} => name
                    | _ => throw self.parse_err("struct/class is expected")
                    }
                val (process, some_class) =
                    if stmt.contains("CV_EXPORTS_W") || stmt.contains("CV_EXPORTS_AS") {
                        (true, Some(class_decl))
                    } else {(false, None)}
                (stmt_type, classname, process, some_class)
            } else if (end_token == "{" && stmt.startswith("enum")) || stmt.startswith("namespace") {
                //NB: Drop inheritance syntax for enum
                val block_type = if stmt.startswith("enum") {BlockEnum} else {BlockNamespace}
                val stmt = stmt.split(':', allow_empty=false).hd()
                val sp = stmt.rfind(' ')
                if sp > 0 {(block_type, stmt[sp+1:].strip(), true, None)}
                else {(block_type, "<unnamed>", true, None)}
            } else if end_token == "{" && stmt.startswith("extern") && stmt.contains("\"C\"") {
                (BlockNamespace, "", true, None)
            } else if end_token == "}" &&
                (context == BlockEnum || context == BlockEnumClass || context == BlockEnumStruct) {
                val elems = self.parse_enum(stmt)
                val name = stack_top.block_name
                (context, name, false, Some(DeclEnum {name=name, elems=elems}))
            } else if end_token == ";" && stmt.startswith("typedef") {
                //TODO: handle typedef's more intelligently
                (stmt_type, "", false, None)
            } else {
                val paren_pos = stmt.find("(")
                if paren_pos >= 0 {
                    //assume it's function or method declaration,
                    //since we filtered off the other places where '(' can normally occur:
                    //  - code blocks
                    //  - function pointer typedef's
                    val decl = self.parse_func_decl(stmt, mat=mat, docstring=docstring)
                    //we return parse_flag == false to prevent the parser to look inside function/method bodies
                    //(except for tracking the nested blocks)
                    (stmt_type, "", false, decl)
                } else if end_token == ";" && (context == BlockClass || context == BlockClass) {
                    //looks like it's member declaration; append the members to the class declaration
                    if stmt.contains("CV_PROP") { //or (class_decl and ("/Map" in class_decl[2])):
                        val readwrite = stmt.contains("CV_PROP_RW")
                        val stmt = batch_replace(stmt, [("CV_PROP_RW", ""), ("CV_PROP", "")]).strip()
                        val var_list = stmt.split(',', allow_empty=false)
                        val (arg_info0, _) = self.parse_arg(var_list.hd(), -1)
                        val class_members = match self.block_stack.hd() {
                            | {decl=Some(DeclClass {members})} => members
                            | _ => throw self.parse_err("invalid context; class/struct is expected")
                        }

                        for v <- (arg_info0.name :: var_list.tl()) {
                            val vv = v.split('=', allow_empty=false)
                            val vname = vv.hd().strip()
                            val vdefval = match vv.tl() {
                                | [] => ""
                                | dv :: [] => dv.strip()
                                | _ => throw self.parse_err("invalid class property (double '=')")
                            }
                            *class_members = arg_info0.{name=vname, defval=vdefval, readwrite=readwrite} :: *class_members
                        }
                    }
                    (stmt_type, "", false, None)
                } else {
                    //something unknown
                    (stmt_type, "", false, None)
                }
            }
        }
    }
}

fun string(a: arg_info_t)
{
    val {name, arg_type, defval, readwrite, isinout, isoutput, isconst, isref, isrref} = a
    val arg_type = (if isinout {"[in/out] "} else {""}) + (if isoutput {"[out] "} else {""}) +
                   (if readwrite {"[read/write] "} else {""})  +
                   (if isconst {"const "} else {""}) + arg_type +
                   (if isref {"&"} else {""}) + (if isrref {"&&"} else {""})
    val defval = if defval == "" {defval} else {f"={defval}"}
    f"{arg_type} {name}{defval}"
}

/*
Prints the list of declarations, retrieived by the parse() method
*/
fun print_decls(decls: decl_t list) =
    for d <- decls {
        | DeclFunc {name, rettype, args, name_alias, static_method, const_method, virtual_method, pure_virtual_method} =>
            val prefix = if static_method {"static "} else if virtual_method {"virtual "} else {""}
            val suffix = if const_method {" const"} else {""}
            val suffix = suffix + (if pure_virtual_method {" = 0"} else {""})
            val alias_str = if name_alias=="" {""} else {f" (as {name_alias})"}
            val args = ",\n\t".join([for a <- args {string(a)}])
            val args = if args == "" {args} else {f"\n\t{args}"}
            println(f"{prefix}func {name}{alias_str}({args}): {rettype}{suffix}")
        | DeclEnum {name, elems} =>
            val elems = ",\n\t".join([for a <- elems {string(a)}])
            val elems = if elems == "" {elems} else {f"\n\t{elems}"}
            println(f"enum {name}{{{elems}}}")
        | DeclClass {name, bases, name_alias, members, ismap, isparams, issimple} =>
            val prefix = if ismap {"map "} else {""}
            val prefix = (if isparams {"params "} else {""}) + prefix
            val prefix = (if issimple {"simple "} else {""}) + prefix
            val bases = if bases == [] {""} else {": " + ", ".join(bases)}
            val alias_str = if name_alias == "" {""} else {f"(as {name_alias})"}
            val members = ";\n\t".join([for a <- *members {string(a)}])
            println(f"{prefix}class {name}{alias_str}{bases}\n{{\n\t{members}\n}}")
    }

fun parse_all(opencv_root: string)
{
    var all_namespaces = Hashset.empty(16, "")
    val all_decls = [for hname <- opencv_hdr_list {
        val (namespaces, decls) = parse(Filename.concat(opencv_root, f"modules/{hname}"))
        all_namespaces.union(namespaces)
        decls
        }]
    val all_decls = all_decls.concat()
    (all_namespaces, all_decls)
}

val opencv_root = match Sys.arguments() {
    | opencv_root :: _ => opencv_root
    | _ => "../opencv"
}

val (all_namespaces, all_decls) = parse_all(opencv_root)
print_decls(all_decls)
val namespace_list = all_namespaces.list().sort((<))
println(f"namespaces: {namespace_list}")
