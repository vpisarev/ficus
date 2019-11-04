open Options

let () =
    let ok = parse_options() in
    if ok then
        ignore(Compiler.process_all options.filename)
    else ()
