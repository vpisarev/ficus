/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    This is a conversion of opencv's hdr_parser.py script that parses OpenCV headers and
    extracts information about API, sufficient to generate bindings for various languages.
    Here is the original OpenCV license: https://github.com/opencv/opencv/blob/master/LICENSE
*/

import Sys, Re, File, Hashset, Hashmap

// the list only for debugging. The real list, used in the real OpenCV build, is specified in CMakeLists.txt
opencv_hdr_list = [
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
]

type block_t = BlockEnumStruct | BlockEnumClass | BlockFile | BlockClass | BlockNamespace | BlockGeneric
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
type param_info_t =
{
    name: string
    paramtype: string
    defval: string
    isinput: bool
    isoutput: bool
}

type decl_t =
    | DeclFunc : {
        name: string
        rettype: string=""
        params: param_info_t list
        orig_rettype: string=""
        docstring: string=""
        export_as: string=""
        phantom: bool=false
        mappable: string=""
        static_method: bool=false
        const_method: bool=false
        virtual_method: bool=false
        pure_virtual_method: bool=false
    }
    | DeclEnum : {
        name: string
        elems: (string, string, string) list
    }
    | DeclClass : {
        name: string
        bases: string list
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
        lineno=0
        wrap_mode=wrap_mode
    }

    var decls: decl_info_t list = []
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
        self.lineno += 1
        //print(state, self.lineno, l0)

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
            if l.endswith("\\"):
                continue
            state = StateScan
            l = re_eol_comment.replace(l, "").strip()  //drop // comment
            match l {
            | "#if 0" | "#if defined(__OPENCV_BUILD)" | "#ifdef __OPENCV_BUILD"
            | "#if !defined(OPENCV_BINDING_PARSER)" | "#ifndef OPENCV_BINDING_PARSER" =>
                state = ScanDirectiveIf0
                depth_if_0 = 1
            | _ => {}
            }
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
            pos = l.find("*/")
            if pos < 0 {
                docstring += l0
                continue
            }
            docstring += l[:pos] + "\n"
            l = l[pos+2:]
            state = StateScan
        }

        if l.startswith('CV__') || l.startswith('__CV_') { //just ignore these lines
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
                | _ => self.find_next_token(l, [";", "\"", "{", "}", "//", "/*"])
            }

            if token == "" {
                block_head += " " + l
                block_head = block_head.strip()
                if block_head != "" && block_head.endswith(')') && block_head.startswith("CV_ENUM_FLAGS(") {
                    l = ""
                    token = ";"
                }
                else {
                    break
                }

            if token == "//" {
                block_head += " " + l[:pos]
                l = ""
                continue
            }

            if token == "/*" {
                block_head += " " + l[:pos]
                val end_pos = l.find("*/", pos+2)
                if l.length() > pos + 2 and l[pos+2] == '*' {
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
                        break
                    }
                    pos2 = pos2_ + 2
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
                    self.namespaces.add(".".join(nested.rev()))
                }
                (block_type, name, parse_flag, decl_opt)
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
            l = l[pos+1:]
        }
    }

    decls.rev()
}

fun hdr_parser_t.get_macro_arg(arg_str: string, npos: int)
{
    val npos2 = arg_str.find("(", npos)
    var npos3 = npos2
    if npos2 < 0 {
        throw self.parse_err("no arguments for the macro")
    }
    val balance = 1
    while true {
        val (t, npos3_) = self.find_next_token(arg_str, ['(', ')'], npos3+1)
        if npos3_ < 0 {
            throw self.parse_err("no matching ')' in the macro call")
        }
        if t == '(' {
            balance += 1
        }
        if t == ')' {
            balance -= 1
            if balance == 0 {
                break
            }
        }
        npos3 = npos3_
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
    var arg_str = arg_str0
    var isinput = true, isoutput = false

    //pass 0: extracts the modifiers
    if arg_str.contains("CV_OUT") {
        isinput = false
        isoutput = true
        arg_str = arg_str.replace("CV_OUT", "")
    }

    if arg_str.contains("CV_IN_OUT") {
        isinput = true
        isoutput = true
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
    var isrref = false

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
        w = arg_str[word_start:npos].strip()
        if w == "operator" {
            word_list.append("operator " + arg_str[npos:].strip())
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
            if prev_w == "char" && !isarray {
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
                throw parse_err("argument contains ',' or '>' not within template arguments")
            }
            if w == "," {
                arg_type += "_and_"
            } else if w == ">" {
                angle_stack = match angle_stack {
                | 0 :: _ =>
                    throw parse_err("template has no arguments")
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
            throw parse_err("no closing ]")
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
        if arg_type.startswith("operator"):
            arg_name = arg_type
            arg_type = ""
        } else {
            arg_name = "arg" + str(argno)
            argno += 1
        }
    }

    while arg_type.endswith("_end_") {
        arg_type = arg_type[:-length("_end_")]
    }

    if add_star {
        arg_type += "*"
    }

    arg_type = batch_replace(arg_type, [("std::", ""), ("cv::", ""), ("::", "_")])
    (arg_type, arg_name, isinput, isoutput, )
    return arg_type, arg_name, modlist, argno

def parse_enum(self, decl_str):
    l = decl_str
    ll = l.split(",")
    if ll[-1].strip() == "":
        ll = ll[:-1]
    prev_val = ""
    prev_val_delta = -1
    decl = []
    for pair in ll:
        pv = pair.split("=")
        if len(pv) == 1:
            prev_val_delta += 1
            val = ""
            if prev_val:
                val = prev_val + "+"
            val += str(prev_val_delta)
        else:
            prev_val_delta = 0
            prev_val = val = pv[1].strip()
        decl.append(["const " + self.get_dotted_name(pv[0].strip()), val, [], [], None, ""])
    return decl

def parse_class_decl(self, decl_str):
    """
    Parses class/struct declaration start in the form:
        {class|struct} [CV_EXPORTS] <class_name> [: public <base_class1> [, ...]]
    Returns class_name1, <list of base_classes>
    """
    l = decl_str
    modlist = []
    if "CV_EXPORTS_W_MAP" in l:
        l = l.replace("CV_EXPORTS_W_MAP", "")
        modlist.append("/Map")
    if "CV_EXPORTS_W_PARAMS" in l:
        l = l.replace("CV_EXPORTS_W_PARAMS", "")
        modlist.append("/Map")
        modlist.append("/Params")
    if "CV_EXPORTS_W_SIMPLE" in l:
        l = l.replace("CV_EXPORTS_W_SIMPLE", "")
        modlist.append("/Simple")
    npos = l.find("CV_EXPORTS_AS")
    if npos < 0:
        npos = l.find('CV_WRAP_AS')
    if npos >= 0:
        macro_arg, npos3 = self.get_macro_arg(l, npos)
        modlist.append("=" + macro_arg)
        l = l[:npos] + l[npos3+1:]

    l = self.batch_replace(l, [("CV_EXPORTS_W", ""), ("CV_EXPORTS", ""), ("public virtual ", " "), ("public ", " "), ("::", ".")]).strip()
    ll = re.split(r'\s+|\s*[,:]\s*', l)
    ll = [le for le in ll if le]
    classname = ll[1]
    bases = ll[2:]
    return classname, bases, modlist

def parse_func_decl_no_wrap(self, decl_str, static_method=false, docstring=""):
    decl_str = (decl_str or "").strip()
    virtual_method = false
    explicit_method = false
    if decl_str.startswith("explicit"):
        decl_str = decl_str[len("explicit"):].lstrip()
        explicit_method = true
    if decl_str.startswith("virtual"):
        decl_str = decl_str[len("virtual"):].lstrip()
        virtual_method = true
    if decl_str.startswith("static"):
        decl_str = decl_str[len("static"):].lstrip()
        static_method = true

    fdecl = decl_str.replace("CV_OUT", "").replace("CV_IN_OUT", "")
    fdecl = fdecl.strip().replace("\t", " ")
    while "  " in fdecl:
        fdecl = fdecl.replace("  ", " ")
    fname = fdecl[:fdecl.find("(")].strip()
    fnpos = fname.rfind(" ")
    if fnpos < 0:
        fnpos = 0
    fname = fname[fnpos:].strip()
    rettype = fdecl[:fnpos].strip()

    if rettype.endswith("operator"):
        fname = ("operator " + fname).strip()
        rettype = rettype[:rettype.rfind("operator")].strip()
        if rettype.endswith("::"):
            rpos = rettype.rfind(" ")
            if rpos >= 0:
                fname = rettype[rpos+1:].strip() + fname
                rettype = rettype[:rpos].strip()
            else:
                fname = rettype + fname
                rettype = ""

    apos = fdecl.find("(")
    if fname.endswith("operator"):
        fname += " ()"
        apos = fdecl.find("(", apos+1)

    fname = "cv." + fname.replace("::", ".")
    decl = [fname, rettype, [], [], None, docstring]

    //inline constructor implementation
    implmatch = re.match(r"(\(.*?\))\s*:\s*(\w+\(.*?\),?\s*)+", fdecl[apos:])
    if bool(implmatch):
        fdecl = fdecl[:apos] + implmatch.group(1)

    args0str = fdecl[apos+1:fdecl.rfind(")")].strip()

    if args0str != "" and args0str != "void":
        args0str = re.sub(r"\([^)]*\)", lambda m: m.group(0).replace(',', "@comma@"), args0str)
        args0 = args0str.split(",")

        args = []
        narg = ""
        for arg in args0:
            narg += arg.strip()
            balance_paren = narg.count("(") - narg.count(")")
            balance_angle = narg.count("<") - narg.count(">")
            if balance_paren == 0 and balance_angle == 0:
                args.append(narg.strip())
                narg = ""

        for arg in args:
            dfpos = arg.find("=")
            defval = ""
            if dfpos >= 0:
                defval = arg[dfpos+1:].strip()
            else:
                dfpos = arg.find("CV_DEFAULT")
                if dfpos >= 0:
                    defval, pos3 = self.get_macro_arg(arg, dfpos)
                else:
                    dfpos = arg.find("CV_WRAP_DEFAULT")
                    if dfpos >= 0:
                        defval, pos3 = self.get_macro_arg(arg, dfpos)
            if dfpos >= 0:
                defval = defval.replace("@comma@", ",")
                arg = arg[:dfpos].strip()
            pos = len(arg)-1
            while pos >= 0 and (arg[pos] in "_[]" or arg[pos].isalpha() or arg[pos].isdigit()):
                pos -= 1
            if pos >= 0:
                aname = arg[pos+1:].strip()
                atype = arg[:pos+1].strip()
                if aname.endswith("&") or aname.endswith("*") or (aname in ["int", "String", "Mat"]):
                    atype = (atype + " " + aname).strip()
                    aname = ""
            else:
                atype = arg
                aname = ""
            if aname.endswith("]"):
                bidx = aname.find('[')
                atype += aname[bidx:]
                aname = aname[:bidx]
            decl[3].append([atype, aname, defval, []])

    if static_method:
        decl[2].append("/S")
    if virtual_method:
        decl[2].append("/V")
    if explicit_method:
        decl[2].append("/E")
    if bool(re.match(r".*\)\s*(const)?\s*=\s*0", decl_str)):
        decl[2].append("/A")
    if bool(re.match(r".*\)\s*const(\s*=\s*0)?", decl_str)):
        decl[2].append("/C")
    return decl

def parse_func_decl(self, decl_str, mat="Mat", docstring=""):
    """
    Parses the function or method declaration in the form:
    [([CV_EXPORTS] <rettype>) | CVAPI(rettype)]
        [~]<function_name>
        (<arg_type1> <arg_name1>[=<default_value1>] [, <arg_type2> <arg_name2>[=<default_value2>] ...])
        [const] {; | <function_body>}

    Returns the function declaration entry:
    [<func name>, <return value C-type>, <list of modifiers>, <list of arguments>, <original return type>, <docstring>] (see above)
    """

    if self.wrap_mode:
        if not (("CV_EXPORTS_AS" in decl_str) or ("CV_EXPORTS_W" in decl_str) or ("CV_WRAP" in decl_str)):
            return []

    //ignore old API in the documentation check (for now)
    if "CVAPI(" in decl_str and self.wrap_mode:
        return []

    top = self.block_stack[-1]
    func_modlist = []

    npos = decl_str.find("CV_EXPORTS_AS")
    if npos >= 0:
        arg, npos3 = self.get_macro_arg(decl_str, npos)
        func_modlist.append("="+arg)
        decl_str = decl_str[:npos] + decl_str[npos3+1:]
    npos = decl_str.find("CV_WRAP_AS")
    if npos >= 0:
        arg, npos3 = self.get_macro_arg(decl_str, npos)
        func_modlist.append("="+arg)
        decl_str = decl_str[:npos] + decl_str[npos3+1:]
    npos = decl_str.find("CV_WRAP_PHANTOM")
    if npos >= 0:
        decl_str, _ = self.get_macro_arg(decl_str, npos)
        func_modlist.append("/phantom")
    npos = decl_str.find("CV_WRAP_MAPPABLE")
    if npos >= 0:
        mappable, npos3 = self.get_macro_arg(decl_str, npos)
        func_modlist.append("/mappable="+mappable)
        classname = top[1]
        return ['.'.join([classname, classname]), None, func_modlist, [], None, None]

    virtual_method = false
    pure_virtual_method = false
    const_method = false

    //filter off some common prefixes, which are meaningless for Python wrappers.
    //note that we do not strip "static" prefix, which does matter;
    //it means class methods, not instance methods
    decl_str = self.batch_replace(decl_str, [("static inline", ""), ("inline", ""), ("explicit ", ""),
                                                ("CV_EXPORTS_W", ""), ("CV_EXPORTS", ""), ("CV_CDECL", ""),
                                                ("CV_WRAP ", " "), ("CV_INLINE", ""),
                                                ("CV_DEPRECATED", ""), ("CV_DEPRECATED_EXTERNAL", "")]).strip()


    if decl_str.strip().startswith('virtual'):
        virtual_method = true

    decl_str = decl_str.replace('virtual' , '')

    end_tokens = decl_str[decl_str.rfind(')'):].split()
    const_method = 'const' in end_tokens
    pure_virtual_method = '=' in end_tokens and '0' in end_tokens

    static_method = false
    context = top[0]
    if decl_str.startswith("static") and (context == "class" or context == "struct"):
        decl_str = decl_str[len("static"):].lstrip()
        static_method = true

    args_begin = decl_str.find("(")
    if decl_str.startswith("CVAPI"):
        rtype_end = decl_str.find(")", args_begin+1)
        if rtype_end < 0:
            print("Error at %d. no terminating ) in CVAPI() macro: %s" % (self.lineno, decl_str))
            sys.exit(-1)
        decl_str = decl_str[args_begin+1:rtype_end] + " " + decl_str[rtype_end+1:]
        args_begin = decl_str.find("(")
    if args_begin < 0:
        print("Error at %d: no args in '%s'" % (self.lineno, decl_str))
        sys.exit(-1)

    decl_start = decl_str[:args_begin].strip()
    //handle operator () case
    if decl_start.endswith("operator"):
        args_begin = decl_str.find("(", args_begin+1)
        if args_begin < 0:
            print("Error at %d: no args in '%s'" % (self.lineno, decl_str))
            sys.exit(-1)
        decl_start = decl_str[:args_begin].strip()
        //TODO: normalize all type of operators
        if decl_start.endswith("()"):
            decl_start = decl_start[0:-2].rstrip() + " ()"

    //constructor/destructor case
    if bool(re.match(r'^(\w+::)*(?P<x>\w+)::~?(?P=x)$', decl_start)):
        decl_start = "void " + decl_start

    rettype, funcname, modlist, argno = self.parse_arg(decl_start, -1)

    //determine original return type, hack for return types with underscore
    original_type = None
    i = decl_start.rfind(funcname)
    if i > 0:
        original_type = decl_start[:i].replace("&", "").replace("const", "").strip()

    if argno >= 0:
        classname = top[1]
        if rettype == classname or rettype == "~" + classname:
            rettype, funcname = "", rettype
        else:
            if bool(re.match('\w+\s+\(\*\w+\)\s*\(.*\)', decl_str)):
                return [] //function typedef
            elif bool(re.match('\w+\s+\(\w+::\*\w+\)\s*\(.*\)', decl_str)):
                return [] //class method typedef
            elif bool(re.match('[A-Z_]+', decl_start)):
                return [] //it seems to be a macro instantiation
            elif "__declspec" == decl_start:
                return []
            elif bool(re.match(r'\w+\s+\(\*\w+\)\[\d+\]', decl_str)):
                return [] //exotic - dynamic 2d array
            else:
                #print rettype, funcname, modlist, argno
                print("Error at %s:%d the function/method name is missing: '%s'" % (self.hname, self.lineno, decl_start))
                sys.exit(-1)

    if self.wrap_mode and (("::" in funcname) or funcname.startswith("~")):
        //if there is :: in function name (and this is in the header file),
        //it means, this is inline implementation of a class method.
        //Thus the function has been already declared within the class and we skip this repeated
        //declaration.
        //Also, skip the destructors, as they are always wrapped
        return []

    funcname = self.get_dotted_name(funcname)

    if not self.wrap_mode:
        decl = self.parse_func_decl_no_wrap(decl_str, static_method, docstring)
        decl[0] = funcname
        return decl

    arg_start = args_begin+1
    npos = arg_start-1
    balance = 1
    angle_balance = 0
    //scan the argument list; handle nested parentheses
    args_decls = []
    args = []
    argno = 1

    while balance > 0:
        npos += 1
        t, npos = self.find_next_token(decl_str, ["(", ")", ",", "<", ">"], npos)
        if not t:
            print("Error: no closing ')' at %d" % (self.lineno,))
            sys.exit(-1)
        if t == "<":
            angle_balance += 1
        if t == ">":
            angle_balance -= 1
        if t == "(":
            balance += 1
        if t == ")":
            balance -= 1

        if (t == "," and balance == 1 and angle_balance == 0) or balance == 0:
            //process next function argument
            a = decl_str[arg_start:npos].strip()
            #print "arg = ", a
            arg_start = npos+1
            if a:
                eqpos = a.find("=")
                defval = ""
                modlist = []
                if eqpos >= 0:
                    defval = a[eqpos+1:].strip()
                else:
                    eqpos = a.find("CV_DEFAULT")
                    if eqpos >= 0:
                        defval, pos3 = self.get_macro_arg(a, eqpos)
                    else:
                        eqpos = a.find("CV_WRAP_DEFAULT")
                        if eqpos >= 0:
                            defval, pos3 = self.get_macro_arg(a, eqpos)
                if defval == "NULL":
                    defval = "0"
                if eqpos >= 0:
                    a = a[:eqpos].strip()
                arg_type, arg_name, modlist, argno = self.parse_arg(a, argno)
                if self.wrap_mode:
                    //TODO: Vectors should contain UMat, but this is not very easy to support and not very needed
                    vector_mat = "vector_{}".format(mat)
                    vector_mat_template = "vector<{}>".format(mat)

                    if arg_type == "InputArray":
                        arg_type = mat
                    elif arg_type == "InputOutputArray":
                        arg_type = mat
                        modlist.append("/IO")
                    elif arg_type == "OutputArray":
                        arg_type = mat
                        modlist.append("/O")
                    elif arg_type == "InputArrayOfArrays":
                        arg_type = vector_mat
                    elif arg_type == "InputOutputArrayOfArrays":
                        arg_type = vector_mat
                        modlist.append("/IO")
                    elif arg_type == "OutputArrayOfArrays":
                        arg_type = vector_mat
                        modlist.append("/O")
                    defval = self.batch_replace(defval, [("InputArrayOfArrays", vector_mat_template),
                                                            ("InputOutputArrayOfArrays", vector_mat_template),
                                                            ("OutputArrayOfArrays", vector_mat_template),
                                                            ("InputArray", mat),
                                                            ("InputOutputArray", mat),
                                                            ("OutputArray", mat),
                                                            ("noArray", arg_type)]).strip()
                args.append([arg_type, arg_name, defval, modlist])
            npos = arg_start-1

    if static_method:
        func_modlist.append("/S")
    if const_method:
        func_modlist.append("/C")
    if virtual_method:
        func_modlist.append("/V")
    if pure_virtual_method:
        func_modlist.append("/PV")

    return [funcname, rettype, func_modlist, args, original_type, docstring]

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
        for b <- self.block_stack {
            val {block_type, block_name} = b
            if block_type == BlockFile || block_type == in ["file", "enum"]:
                continue
            if block_type in ["enum struct", "enum class"] and block_name == name:
                continue
            if block_type not in ["struct", "class", "namespace", "enum struct", "enum class"]:
                print("Error at %d: there are non-valid entries in the current block stack %s" % (self.lineno, self.block_stack))
                sys.exit(-1)
            if block_name and (block_type == "namespace" or not qualified_name):
                n += block_name + "."
        n += name.replace("::", ".")
        if n.endswith(".Algorithm"):
            n = "cv.Algorithm"
        return n
    }

def parse_stmt(self, stmt, end_token, mat="Mat", docstring=""):
    """
    parses the statement (ending with ';' or '}') or a block head (ending with '{')

    The function calls parse_class_decl or parse_func_decl when necessary. It returns
    <block_type>, <block_name>, <parse_flag>, <declaration>
    where the first 3 values only make sense for blocks (i.e. code blocks, namespaces, classes, enums and such)
    """
    stack_top = self.block_stack[-1]
    context = stack_top[self.BLOCK_TYPE]

    if stmt.startswith('inline namespace'):
        //emulate anonymous namespace
        return "namespace", "", true, None

    stmt_type = ""
    if end_token == "{":
        stmt_type = "block"

    if context == "block":
        print("Error at %d: should not call parse_stmt inside blocks" % (self.lineno,))
        sys.exit(-1)

    if context == "class" or context == "struct":
        while 1:
            colon_pos = stmt.find(":")
            if colon_pos < 0:
                break
            w = stmt[:colon_pos].strip()
            if w in ["public", "protected", "private"]:
                if w == "public" or (not self.wrap_mode and w == "protected"):
                    stack_top[self.PUBLIC_SECTION] = true
                else:
                    stack_top[self.PUBLIC_SECTION] = false
                stmt = stmt[colon_pos+1:].strip()
            break

    //do not process hidden class members and template classes/functions
    if not stack_top[self.PUBLIC_SECTION] or stmt.startswith("template"):
        return stmt_type, "", false, None

    if end_token == "{":
        if not self.wrap_mode and stmt.startswith("typedef struct"):
            stmt_type = "struct"
            try:
                classname, bases, modlist = self.parse_class_decl(stmt[len("typedef "):])
            except:
                print("Error at %s:%d" % (self.hname, self.lineno))
                exit(1)
            if classname.startswith("_Ipl"):
                classname = classname[1:]
            decl = [stmt_type + " " + self.get_dotted_name(classname), "", modlist, [], None, docstring]
            if bases:
                decl[1] = ": " + ", ".join([self.get_dotted_name(b).replace(".","::") for b in bases])
            return stmt_type, classname, true, decl

        if stmt.startswith("class") or stmt.startswith("struct"):
            stmt_type = stmt.split()[0]
            if stmt.strip() != stmt_type:
                try:
                    classname, bases, modlist = self.parse_class_decl(stmt)
                except:
                    print("Error at %s:%d" % (self.hname, self.lineno))
                    exit(1)
                decl = []
                if ("CV_EXPORTS_W" in stmt) or ("CV_EXPORTS_AS" in stmt) or (not self.wrap_mode):# and ("CV_EXPORTS" in stmt)):
                    decl = [stmt_type + " " + self.get_dotted_name(classname), "", modlist, [], None, docstring]
                    if bases:
                        decl[1] = ": " + ", ".join([self.get_dotted_name(b).replace(".","::") for b in bases])
                return stmt_type, classname, true, decl

        if stmt.startswith("enum") or stmt.startswith("namespace"):
            //NB: Drop inheritance syntax for enum
            stmt = stmt.split(':')[0]
            stmt_list = stmt.rsplit(" ", 1)
            if len(stmt_list) < 2:
                stmt_list.append("<unnamed>")
            return stmt_list[0], stmt_list[1], true, None

        if stmt.startswith("extern") and "\"C\"" in stmt:
            return "namespace", "", true, None

    if end_token == "}" and context.startswith("enum"):
        decl = self.parse_enum(stmt)
        name = stack_top[self.BLOCK_NAME]
        return context, name, false, decl

    if end_token == ";" and stmt.startswith("typedef"):
        //TODO: handle typedef's more intelligently
        return stmt_type, "", false, None

    paren_pos = stmt.find("(")
    if paren_pos >= 0:
        //assume it's function or method declaration,
        //since we filtered off the other places where '(' can normally occur:
        //  - code blocks
        //  - function pointer typedef's
        decl = self.parse_func_decl(stmt, mat=mat, docstring=docstring)
        //we return parse_flag == false to prevent the parser to look inside function/method bodies
        //(except for tracking the nested blocks)
        return stmt_type, "", false, decl

    if (context == "struct" or context == "class") and end_token == ";" and stmt:
        //looks like it's member declaration; append the members to the class declaration
        class_decl = stack_top[self.CLASS_DECL]
        if ("CV_PROP" in stmt): //or (class_decl and ("/Map" in class_decl[2])):
            var_modlist = []
            if "CV_PROP_RW" in stmt:
                var_modlist.append("/RW")
            stmt = self.batch_replace(stmt, [("CV_PROP_RW", ""), ("CV_PROP", "")]).strip()
            var_list = stmt.split(",")
            var_type, var_name1, modlist, argno = self.parse_arg(var_list[0], -1)
            var_list = [var_name1] + [i.strip() for i in var_list[1:]]

            for v in var_list:
                vv = v.split("=")
                vname = vv[0].strip()
                vdefval = ""
                if len(vv) > 1:
                    vdefval = vv[1].strip()
                class_decl[3].append([var_type, vname, vdefval, var_modlist])
        return stmt_type, "", false, None

    //something unknown
    return stmt_type, "", false, None

def find_next_token(self, s, tlist, p=0):
    """
    Finds the next token from the 'tlist' in the input 's', starting from position 'p'.
    Returns the first occurred token and its position, or ("", len(s)) when no token is found
    """
    token = ""
    tpos = len(s)
    for t in tlist:
        pos = s.find(t, p)
        if pos < 0:
            continue
        if pos < tpos:
            tpos = pos
            token = t
    return token, tpos

def print_decls(self, decls):
    """
    Prints the list of declarations, retrieived by the parse() method
    """
    for d in decls:
        print(d[0], d[1], ";".join(d[2]))
        //Uncomment below line to see docstrings
        //print('"""\n' + d[5] + '\n"""')
        for a in d[3]:
            print("   ", a[0], a[1], a[2], end="")
            if a[3]:
                print("; ".join(a[3]))
            else:
                print()

if __name__ == '__main__':
    parser = CppHeaderParser(generate_umat_decls=true, generate_gpumat_decls=true)
    decls = []
    for hname in opencv_hdr_list:
        decls += parser.parse(hname)
    #for hname in sys.argv[1:]:
        #decls += parser.parse(hname, wmode=false)
    parser.print_decls(decls)
    print(len(decls))
    print("namespaces:", " ".join(sorted(parser.namespaces)))
