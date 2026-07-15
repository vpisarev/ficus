"""
lsp-1 Phase 1 test driver: drive tools/FicusLsp.fx over a stdio pipe and assert
the JSON-RPC / LSP behavior. Python-3-stdlib only (unittest), matching the fxtest
house style. Run standalone (python3 tools/fxtest/lsp_driver.py <ficus> <server>)
or via `fxtest.py lsp`.
"""

import json
import os
import subprocess
import sys
import tempfile
import unittest


class LspClient:
    """A minimal JSON-RPC-over-stdio client for talking to the server binary."""

    def __init__(self, server, ficus_path):
        self.p = subprocess.Popen(
            [server], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        self.ficus_path = ficus_path
        self._id = 0

    # --- framing ---
    def _send(self, obj):
        body = json.dumps(obj).encode("utf-8")
        header = f"Content-Length: {len(body)}\r\n\r\n".encode("ascii")
        self.p.stdin.write(header + body)
        self.p.stdin.flush()

    def _read_message(self):
        # read headers
        headers = {}
        while True:
            line = self.p.stdout.readline()
            if not line:
                raise EOFError("server closed the pipe")
            line = line.decode("utf-8").rstrip("\r\n")
            if line == "":
                break
            k, _, v = line.partition(":")
            headers[k.strip().lower()] = v.strip()
        n = int(headers["content-length"])
        body = self.p.stdout.read(n)
        return json.loads(body.decode("utf-8"))

    # --- requests / notifications ---
    def request(self, method, params):
        self._id += 1
        self._send({"jsonrpc": "2.0", "id": self._id,
                    "method": method, "params": params})
        # skip any server-initiated notifications until our matching response
        while True:
            msg = self._read_message()
            if msg.get("id") == self._id:
                return msg

    def notify(self, method, params):
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    def read_notification(self, method):
        while True:
            msg = self._read_message()
            if msg.get("method") == method:
                return msg

    # --- lifecycle helpers ---
    def initialize(self, root_uri=None):
        return self.request("initialize", {
            "processId": os.getpid(),
            "rootUri": root_uri,
            "capabilities": {},
            "initializationOptions": {"ficusPath": self.ficus_path},
        })

    def close(self):
        try:
            self.request("shutdown", {})
            self.notify("exit", {})
        except Exception:
            pass
        try:
            self.p.wait(timeout=5)
        except Exception:
            self.p.kill()
        return self.p.returncode


def uri(path):
    return "file://" + os.path.abspath(path)


class LspTests(unittest.TestCase):
    FICUS = None
    SERVER = None

    def setUp(self):
        self.tmp = tempfile.mkdtemp(prefix="ficuslsp_")
        self.cli = LspClient(self.SERVER, self.FICUS)
        r = self.cli.initialize(root_uri=uri("."))
        self.assertEqual(r["result"]["capabilities"]["codeActionProvider"], True)
        self.assertEqual(
            r["result"]["capabilities"]["textDocumentSync"]["save"], True)
        self.cli.notify("initialized", {})

    def tearDown(self):
        self.cli.close()

    def _write(self, name, text):
        path = os.path.join(self.tmp, name)
        with open(path, "w") as f:
            f.write(text)
        return path

    def _open(self, path):
        self.cli.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "ficus", "version": 1,
            "text": open(path).read()}})
        return self.cli.read_notification("textDocument/publishDiagnostics")

    # --- the cases the brief calls for ---

    def test_typo_one_diagnostic_and_quickfix(self):
        path = self._write("typo.fx",
                           "val doubled = 21 * 2\nval r = lenght + doubled\nprintln(r)\n")
        pub = self._open(path)
        diags = pub["params"]["diagnostics"]
        self.assertEqual(len(diags), 1)
        d = diags[0]
        self.assertEqual(d["severity"], 1)                 # error
        # exact span: line 1 (0-based), 'lenght' at chars 8..14
        self.assertEqual(d["range"]["start"], {"line": 1, "character": 8})
        self.assertEqual(d["range"]["end"], {"line": 1, "character": 14})
        self.assertIn("length", d["message"])
        self.assertEqual(d["data"]["suggestions"], ["length"])
        # codeAction turns the suggestion into a quickfix
        actions = self.cli.request("textDocument/codeAction", {
            "textDocument": {"uri": uri(path)},
            "range": d["range"],
            "context": {"diagnostics": [d]},
        })["result"]
        self.assertEqual(len(actions), 1)
        a = actions[0]
        self.assertEqual(a["kind"], "quickfix")
        self.assertEqual(a["title"], "Change to 'length'")
        edit = a["edit"]["changes"][uri(path)][0]
        self.assertEqual(edit["newText"], "length")
        self.assertEqual(edit["range"], d["range"])

    def test_fix_clears_diagnostics(self):
        path = self._write("fix.fx", "val r = lenght\nprintln(r)\n")
        pub = self._open(path)
        self.assertEqual(len(pub["params"]["diagnostics"]), 1)
        # fix it on disk and re-save
        with open(path, "w") as f:
            f.write("val r = 42\nprintln(r)\n")
        self.cli.notify("textDocument/didSave",
                        {"textDocument": {"uri": uri(path)}})
        pub2 = self.cli.read_notification("textDocument/publishDiagnostics")
        self.assertEqual(pub2["params"]["diagnostics"], [])   # cleared

    def test_warning_severity(self):
        # -Wall: a module-level fn with an inferred return type warns (exit 0)
        path = self._write("warn.fx",
                           "fun norm(x: double, y: double) = sqrt(x*x + y*y)\n"
                           "println(norm(3.0, 4.0))\n")
        pub = self._open(path)
        diags = pub["params"]["diagnostics"]
        self.assertTrue(len(diags) >= 1)
        self.assertTrue(any(d["severity"] == 2 for d in diags))  # warning

    def test_clean_file_no_diagnostics(self):
        path = self._write("clean.fx", "val x: int = 40 + 2\nprintln(x)\n")
        pub = self._open(path)
        self.assertEqual(pub["params"]["diagnostics"], [])

    def test_server_survives_bad_input(self):
        # a garbage notification must not kill the server
        self.cli.notify("$/nonsense", {"foo": [1, 2, 3]})
        path = self._write("after.fx", "val x: int = 1\nprintln(x)\n")
        pub = self._open(path)
        self.assertEqual(pub["params"]["diagnostics"], [])

    def test_imported_module_diagnostics_routed_to_own_uri(self):
        # A compile pulls in imports, so a warning in an imported module must be
        # published under THAT module's uri -- not drawn in the opened file.
        helper = self._write("helpermod.fx", "fun twice(x: int) = x*2\n")
        main = self._write(
            "usesit.fx",
            "import helpermod\nval y = helpermod.twice(21)\nprintln(y)\n")
        self.cli.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(main), "languageId": "ficus", "version": 1,
            "text": open(main).read()}})
        pubs = {}
        for _ in range(2):  # one publish for main, one for the imported module
            p = self.cli.read_notification("textDocument/publishDiagnostics")
            pubs[p["params"]["uri"]] = p["params"]["diagnostics"]
        # the opened file is clean; the warning lands on the module's own uri
        self.assertEqual(pubs.get(uri(main)), [])
        self.assertEqual(len(pubs.get(uri(helper), [])), 1)
        self.assertEqual(pubs[uri(helper)][0]["severity"], 2)

    def test_analyzing_leaf_module_does_not_clear_importer(self):
        # Repro of the cross-root clearing bug: open an importer (both it and its
        # imported leaf module have warnings), then open/save the LEAF on its own.
        # Analyzing the leaf must NOT wipe the importer's diagnostics.
        leaf = self._write("leafmod.fx", "fun twice(x: int) = x*2\n")
        root = self._write(
            "rootmod.fx",
            "import leafmod\nfun mainfn(x: int) = x+1\n"
            "val y = leafmod.twice(mainfn(21))\nprintln(y)\n")

        def notify_open(path):
            self.cli.notify("textDocument/didOpen", {"textDocument": {
                "uri": uri(path), "languageId": "ficus", "version": 1,
                "text": open(path).read()}})

        notify_open(root)                       # publishes for root and leaf
        for _ in range(2):
            self.cli.read_notification("textDocument/publishDiagnostics")

        notify_open(leaf)                        # analyze the leaf on its own
        self.cli.notify("textDocument/didSave",  # then re-save the importer
                        {"textDocument": {"uri": uri(root)}})
        pubs = []
        for _ in range(3):                       # fixed server emits exactly 1 + 2
            p = self.cli.read_notification("textDocument/publishDiagnostics")
            pubs.append((p["params"]["uri"], p["params"]["diagnostics"]))

        # the importer must never have been cleared to empty by the leaf analysis
        cleared = [u for (u, d) in pubs if u == uri(root) and d == []]
        self.assertEqual(cleared, [], "leaf analysis wiped the importer's diagnostics")
        root_pubs = [d for (u, d) in pubs if u == uri(root)]
        self.assertTrue(root_pubs and len(root_pubs[-1]) >= 1)

    def test_compiler_failure_reports_internal_error(self):
        # point the server at a missing compiler -> the child exits nonzero with
        # no parseable jsonl -> the server survives and reports ONE internal-error
        # diagnostic (stands in for a real compiler crash; none is fenced today).
        cli = LspClient(self.SERVER, "/nonexistent/ficus-xyz")
        try:
            cli.request("initialize", {
                "processId": os.getpid(), "rootUri": uri("."), "capabilities": {},
                "initializationOptions": {"ficusPath": "/nonexistent/ficus-xyz"}})
            cli.notify("initialized", {})
            path = self._write("boom.fx", "val x: int = 1\nprintln(x)\n")
            cli.notify("textDocument/didOpen", {"textDocument": {
                "uri": uri(path), "languageId": "ficus", "version": 1,
                "text": open(path).read()}})
            pub = cli.read_notification("textDocument/publishDiagnostics")
            diags = pub["params"]["diagnostics"]
            self.assertEqual(len(diags), 1)
            self.assertEqual(diags[0]["severity"], 1)
            self.assertIn("internal compiler error", diags[0]["message"])
        finally:
            cli.close()


def load_suite(ficus, server):
    LspTests.FICUS = ficus
    LspTests.SERVER = server
    return unittest.defaultTestLoader.loadTestsFromTestCase(LspTests)


if __name__ == "__main__":
    ficus = sys.argv[1] if len(sys.argv) > 1 else "bin/ficus"
    server = sys.argv[2] if len(sys.argv) > 2 else "ficus-lsp"
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(load_suite(os.path.abspath(ficus), os.path.abspath(server)))
    sys.exit(0 if result.wasSuccessful() else 1)
