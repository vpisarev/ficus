/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// FicusLsp -- a minimal Language Server for Ficus (lsp-1, Phase 1).
//
// It speaks JSON-RPC 2.0 over stdio (Content-Length framing) and, on
// didOpen/didSave, runs `bin/ficus -no-c -diag-format=json -Wall <file>` as a
// child process, parses the jsonl diagnostics (Phase 0), and republishes them as
// LSP `textDocument/publishDiagnostics`. Each did-you-mean suggestion becomes a
// `codeAction` quickfix that replaces the (exact) identifier span.
//
// Design: a single-threaded, synchronous loop -- each analysis runs to completion
// (~0.13s) before the next message is read, so an unbounded pileup of compiler
// processes is impossible by construction (no async spawn). Isolation and
// crash-safety come free from the process model: a compiler crash surfaces as one
// internal-error diagnostic, it can never take the server down.
//
// v1 subset: initialize/initialized/shutdown/exit lifecycle; textDocumentSync
// {openClose, save}; codeAction. didChange is accepted and ignored (sync on
// save); unsaved-buffer analysis is v1.5 (needs a compiler -substitute-file flag).

import File, Json, Sys, Filename, String, Re

//////////////////// server state (single instance) ////////////////////

var g_ficus_path = "bin/ficus"
var g_root_path = ""
var g_shutdown = false
// URIs we last published NON-EMPTY diagnostics to, so a later analysis can clear
// a file whose diagnostics have gone away (e.g. an imported module got fixed).
var g_published: list[string] = []
val g_log = Sys.getenv("FICUS_LSP_LOG", "") != ""

// Content-Length header, case-insensitive, capturing the byte count.
val g_clen_re = Re.compile(r"[Cc]ontent-[Ll]ength:\s*(\d+)")

fun log(s: string): void =
    if g_log { File.stderr.print("[ficus-lsp] " + s + "\n"); File.stderr.flush() }

//////////////////// tiny JSON navigation helpers ////////////////////

// child value by key (Null if absent / not an object) -- composes for nesting
fun jget(js: Json.t, key: string): Json.t =
    match js {
    | Json.Map(m) => (match m.assoc_opt(key) { | Some(v) => v | _ => Json.Null })
    | _ => Json.Null
    }
fun jstr_of(js: Json.t): string = match js { | Json.Str(s) => s | _ => "" }
fun jint_of(js: Json.t): int =
    match js { | Json.Int(i) => int(i) | Json.Real(r) => int(r) | _ => 0 }
fun jarr_of(js: Json.t): Json.t [] = match js { | Json.Seq(a) => a | _ => [] }

fun jobj(fields: (string, Json.t) []): Json.t = Json.Map(fields)
fun jarr(items: list[Json.t]): Json.t = Json.Seq([for x <- items {x}])
fun jint(i: int): Json.t = Json.Int(int64(i))

//////////////////// stdio framing (byte-exact, Phase 0.5) ////////////////////

// read exactly n bytes from stdin, decode as UTF-8 (LSP frames in BYTES)
fun read_exact(n: int): string {
    if n <= 0 { "" }
    else {
        val buf = array(n, 0u8)
        val got = File.stdin.read(buf)
        String.from_utf8(if got == n { buf } else { buf[:got] })
    }
}

// read one framed message; None at EOF (client closed the pipe)
fun read_message(): string? {
    var content_length = -1
    var in_headers = true
    while in_headers {
        val raw = File.stdin.readln()
        if raw == "" { return None }          // EOF (fgets -> NULL)
        val line = raw.rstrip()               // drop trailing \r\n
        if line == "" { in_headers = false }  // blank line ends the headers
        else {
            match g_clen_re.find_str(line) {
            | Some(m) when m.size() >= 2 =>
                content_length = match m[1].to_int() { | Some(v) => v | _ => -1 }
            | _ => {}                          // ignore other headers
            }
        }
    }
    if content_length < 0 { Some("") } else { Some(read_exact(content_length)) }
}

fun write_message(payload: string): void {
    val n = String.utf8_length(payload)        // Content-Length is BYTES, not chars
    File.stdout.print(f"Content-Length: {n}\r\n\r\n")
    File.stdout.print(payload)
    File.stdout.flush()
}

fun send(js: Json.t): void = write_message(Json.string(js, compact=true))

fun respond(id: Json.t, result: Json.t): void =
    send(jobj([("jsonrpc", Json.Str("2.0")), ("id", id), ("result", result)]))

fun respond_error(id: Json.t, code: int, message: string): void =
    send(jobj([("jsonrpc", Json.Str("2.0")), ("id", id),
               ("error", jobj([("code", jint(code)), ("message", Json.Str(message))]))]))

//////////////////// URI <-> path ////////////////////

fun hexval(c: char): int =
    if '0' <= c <= '9' { ord(c) - ord('0') }
    else if 'a' <= c <= 'f' { ord(c) - ord('a') + 10 }
    else if 'A' <= c <= 'F' { ord(c) - ord('A') + 10 }
    else { 0 }

// percent-decode (ASCII-correct: %XX -> one char). Non-ASCII percent-encoded
// paths are a v1 limitation; typical POSIX file:// URIs are ASCII + %20 etc.
fun percent_decode(s: string): string {
    val n = s.length()
    var out: list[char] = []
    var i = 0
    while i < n {
        val c = s[i]
        if c == '%' && i + 2 < n {
            out = chr(hexval(s[i+1]) * 16 + hexval(s[i+2])) :: out
            i += 3
        } else {
            out = c :: out
            i += 1
        }
    }
    string([for c <- out.rev() {c}])
}

fun uri_to_path(uri: string): string {
    val u = if uri.startswith("file://") { uri["file://".length():] } else { uri }
    percent_decode(u)
}

// path -> file:// URI (minimal percent-encoding; '%' first so we don't double-
// encode). Used for diagnostics that belong to an imported module, not the file
// the client opened -- for the opened file we reuse the client's exact URI.
fun path_to_uri(path: string): string =
    "file://" + path.replace("%", "%25").replace(" ", "%20")
                    .replace("#", "%23").replace("?", "%3F")

//////////////////// diagnostics ////////////////////

type ficus_diag_t =
{
    file: string;
    line0: int; col0: int; line1: int; col1: int;
    precision: string; severity: string; message: string; anchor: string;
    suggestions: list[string]
}

fun parse_ficus_diag(line: string): ficus_diag_t? {
    if !line.startswith("{") { None }
    else {
        try {
            val js = Json.parse_string("diag", line)
            Some(ficus_diag_t {
                file = jstr_of(jget(js, "file")),
                line0 = jint_of(jget(js, "line0")), col0 = jint_of(jget(js, "col0")),
                line1 = jint_of(jget(js, "line1")), col1 = jint_of(jget(js, "col1")),
                precision = jstr_of(jget(js, "precision")),
                severity = jstr_of(jget(js, "severity")),
                message = jstr_of(jget(js, "message")),
                anchor = jstr_of(jget(js, "anchor")),
                suggestions = [:: for sj <- jarr_of(jget(js, "suggestions")) { jstr_of(sj) }]
            })
        } catch { | _ => None }
    }
}

fun shell_quote(s: string): string = "'" + s.replace("'", "'\\''") + "'"

// spawn the compiler in json mode; drain the full jsonl; return parsed
// diagnostics, the raw lines (for a crash fallback) and the child exit code.
fun run_analysis(path: string): (list[ficus_diag_t], list[string], int) {
    val cmd = f"{g_ficus_path} -no-c -diag-format=json -Wall {shell_quote(path)}"
    log(f"analyze: {cmd}")
    val p = File.popen(cmd, "r")
    var raw: list[string] = []
    while true {
        val line = p.readln()
        if line == "" { break }
        raw = line.rstrip() :: raw
    }
    val code = p.pclose_exit_status()
    val raw = raw.rev()
    var diags: list[ficus_diag_t] = []
    for ln <- raw { match parse_ficus_diag(ln) { | Some(d) => diags = d :: diags | _ => {} } }
    (diags.rev(), raw, code)
}

fun lsp_range(l0: int, c0: int, l1: int, c1: int): Json.t =
    jobj([("start", jobj([("line", jint(l0)), ("character", jint(c0))])),
          ("end",   jobj([("line", jint(l1)), ("character", jint(c1))]))])

fun to_lsp_diagnostic(d: ficus_diag_t): Json.t {
    val sev = if d.severity == "warning" { 2 } else { 1 }
    // Ficus locs are 1-based, span [col0, col1); LSP is 0-based. For exact
    // precision use the real span; otherwise (drifted location) cover the whole
    // line and fold the anchor into the message.
    val (range, msg) =
        if d.precision == "exact" {
            (lsp_range(d.line0-1, d.col0-1, d.line1-1, d.col1-1), d.message)
        } else {
            val m = if d.anchor != "" { d.message + " (" + d.anchor + ")" } else { d.message }
            (lsp_range(d.line0-1, 0, d.line0, 0), m)
        }
    jobj([
        ("range", range),
        ("severity", jint(sev)),
        ("source", Json.Str("ficus")),
        ("message", Json.Str(msg)),
        // round-trip the suggestions so codeAction can build quickfixes statelessly
        ("data", jobj([("suggestions", jarr([:: for s <- d.suggestions { Json.Str(s) }]))]))
    ])
}

fun internal_error_diag(text: string): Json.t =
    jobj([("range", lsp_range(0, 0, 0, 0)), ("severity", jint(1)),
          ("source", Json.Str("ficus")),
          ("message", Json.Str("ficus: internal compiler error\n" + text)),
          ("data", jobj([("suggestions", jarr([]))]))])

fun publish(uri: string, diags: list[Json.t]): void =
    send(jobj([("jsonrpc", Json.Str("2.0")),
               ("method", Json.Str("textDocument/publishDiagnostics")),
               ("params", jobj([("uri", Json.Str(uri)), ("diagnostics", jarr(diags))]))]))

fun analyze_and_publish(uri: string): void {
    val main_path = uri_to_path(uri)
    val (diags, raw, code) = run_analysis(main_path)
    // A compile pulls in the whole import graph, so diagnostics can belong to
    // OTHER files (imported modules). Group by the diagnostic's own `file` and
    // publish each group under its own URI -- otherwise a warning in an imported
    // module would be drawn in the file the client happens to have open. The
    // opened file reuses the client's exact URI (so it matches regardless of path
    // normalization); imported modules get a synthesized file:// URI.
    fun diag_uri(d: ficus_diag_t): string =
        if d.file == main_path { uri } else { path_to_uri(d.file) }

    // distinct target URIs, always including the opened file (so it clears)
    var target_uris = [:: uri]
    for d <- diags {
        val du = diag_uri(d)
        if !target_uris.mem(du) { target_uris = du :: target_uris }
    }

    var published_now: list[string] = []
    for du <- target_uris {
        val group0 = diags.filter(fun (d) { diag_uri(d) == du }).map(to_lsp_diagnostic)
        // a nonzero exit with no parseable diagnostics anywhere = compiler crash
        val group = if du == uri && group0 == [] && diags == [] && code != 0 {
                        [:: internal_error_diag("\n".join(raw))]
                    } else { group0 }
        publish(du, group)
        if group != [] { published_now = du :: published_now }
    }
    // clear any file we published to last time that has no diagnostics now
    for old <- g_published {
        if !target_uris.mem(old) { publish(old, []) }
    }
    g_published = published_now
}

//////////////////// request handlers ////////////////////

fun resolve_ficus(): string =
    if g_root_path != "" && Filename.exists(g_root_path + "/bin/ficus") { g_root_path + "/bin/ficus" }
    else if Filename.exists("bin/ficus") { "bin/ficus" }
    else { "ficus" }

fun handle_initialize(id: Json.t, params: Json.t): void {
    val fp = jstr_of(jget(jget(params, "initializationOptions"), "ficusPath"))
    val root_uri = jstr_of(jget(params, "rootUri"))
    val root_path = jstr_of(jget(params, "rootPath"))
    g_root_path = if root_uri != "" { uri_to_path(root_uri) } else { root_path }
    g_ficus_path = if fp != "" { fp } else { resolve_ficus() }
    log(f"initialize: ficus='{g_ficus_path}' root='{g_root_path}'")
    val caps = jobj([
        ("textDocumentSync", jobj([("openClose", Json.Bool(true)), ("save", Json.Bool(true))])),
        ("codeActionProvider", Json.Bool(true))
    ])
    respond(id, jobj([
        ("capabilities", caps),
        ("serverInfo", jobj([("name", Json.Str("ficus-lsp")), ("version", Json.Str("0.1"))]))
    ]))
}

fun handle_code_action(id: Json.t, params: Json.t): void {
    val uri = jstr_of(jget(jget(params, "textDocument"), "uri"))
    val ctx_diags = jarr_of(jget(jget(params, "context"), "diagnostics"))
    var actions: list[Json.t] = []
    for dj <- ctx_diags {
        val range = jget(dj, "range")
        val suggestions = jarr_of(jget(jget(dj, "data"), "suggestions"))
        for sj <- suggestions {
            val s = jstr_of(sj)
            if s != "" {
                val edit = jobj([("changes", jobj([(uri, jarr([:: jobj([
                    ("range", range), ("newText", Json.Str(s))])]))]))])
                actions = jobj([
                    ("title", Json.Str(f"Change to '{s}'")),
                    ("kind", Json.Str("quickfix")),
                    ("diagnostics", jarr([:: dj])),
                    ("edit", edit)
                ]) :: actions
            }
        }
    }
    respond(id, jarr(actions.rev()))
}

// returns false when the loop should terminate (on "exit")
fun dispatch(body: string): bool {
    val js = Json.parse_string("msg", body)
    val method = jstr_of(jget(js, "method"))
    val id = jget(js, "id")
    val params = jget(js, "params")
    match method {
    | "initialize" => handle_initialize(id, params); true
    | "initialized" => true
    | "shutdown" => g_shutdown = true; respond(id, Json.Null); true
    | "exit" =>
        // JSON-RPC: exit 0 iff a shutdown request preceded exit, else 1.
        log("exit"); throw Exit(if g_shutdown { 0 } else { 1 })
    | "textDocument/didOpen" => analyze_and_publish(jstr_of(jget(jget(params, "textDocument"), "uri"))); true
    | "textDocument/didSave" => analyze_and_publish(jstr_of(jget(jget(params, "textDocument"), "uri"))); true
    | "textDocument/didChange" => true          // accepted, ignored (v1 syncs on save)
    | "textDocument/didClose" => publish(jstr_of(jget(jget(params, "textDocument"), "uri")), []); true
    | "textDocument/codeAction" => handle_code_action(id, params); true
    | _ =>
        // unknown request -> method-not-found; unknown notification -> ignore
        match id { | Json.Null => {} | _ => respond_error(id, -32601, f"method not found: {method}") }
        true
    }
}

fun serve(): void {
    var running = true
    while running {
        match read_message() {
        | None => running = false                // EOF -> stop
        | Some(body) =>
            if body == "" { continue }           // malformed frame; skip
            // a handler failure must not kill the server -- but let Exit through
            running = try { dispatch(body) }
                      catch { | Exit(c) => throw Exit(c) | e => log(f"dispatch error: {e}"); true }
        }
    }
    log("server stopped")
}

serve()
