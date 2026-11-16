# Copyright (C) 2026 piotr-yuxuan
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
"""A scripted stand-in for `dbt lsp`, on the standard input and output.

It answers `initialize` with capabilities like those of dbt lsp 2.0.9,
formatting and code actions included, and every other request from a JSON
file of answers, whose keys are a method, "workspace/executeCommand
COMMAND" for a command, or "callHierarchy/incomingCalls ID" and
"callHierarchy/outgoingCalls ID" for the calls of the item whose
data.uniqueId is ID: a request without an answer there gets null.  After
`initialized`, it sends the notifications of its own that dbt lsp sends, or
those that the key "$notifications" of the answers lists.  The key
"$experimental" gives its capabilities an experimental object, the key
"$capabilities" adds capabilities, as dbt-lsp-plus adds
callHierarchyProvider, the key "$commands" adds commands to those it lists,
as dbt-lsp-plus adds its own, the key "$errors" maps a key of the answers
to the error that answers it instead, and the key "$delays" maps a method
to the seconds it waits before it answers it.

Environment:
- DBT_MODE_FAKE_ANSWERS: the file of answers, optional;
- DBT_MODE_FAKE_LOG: a file to which it appends its command line, then each
  message it receives, one JSON value per line; optional.
"""

from __future__ import annotations

import json
import os
import sys
import time
from pathlib import Path
from typing import BinaryIO

JSON = None | bool | int | float | str | list["JSON"] | dict[str, "JSON"]

CAPABILITIES = {
    "codeActionProvider": {
        "codeActionKinds": ["source.fixAll", "source.fixAll.dbtLintFix"]
    },
    "codeLensProvider": {"resolveProvider": False},
    "completionProvider": {
        "resolveProvider": False,
        "triggerCharacters": ["{", "'", "."],
    },
    "definitionProvider": True,
    "documentFormattingProvider": True,
    # Announced as false, which lsp-mode takes for support all the same.
    "documentSymbolProvider": False,
    "executeCommandProvider": {
        "commands": ["dbt.listNodes", "dbt.compileFile", "dbt.show"]
    },
    "hoverProvider": True,
    "referencesProvider": True,
    "renameProvider": True,
    "textDocumentSync": {"change": 2, "openClose": True, "save": {}},
    "workspace": {
        "fileOperations": {
            "willRename": {
                "filters": [
                    {
                        "pattern": {"glob": "**/*.{sql,csv}", "matches": "file"},
                        "scheme": "file",
                    }
                ],
            },
        },
    },
}

NOTIFICATIONS = [
    "dbt/lspCompileStart",
    "dbt/lspCompileComplete",
    "dbt/lspBackgroundCompileStart",
    "dbt/lspBackgroundCompileComplete",
    "dbt/renameModel",
]


def read_message(stream: BinaryIO) -> dict[str, JSON] | None:
    """Return the next message of STREAM, or None at its end."""
    length = None
    while line := stream.readline():
        name, _, value = line.decode().strip().partition(":")
        if not name:
            return json.loads(stream.read(length)) if length is not None else None
        if name.lower() == "content-length":
            length = int(value)
    return None


def write_message(message: dict[str, JSON]) -> None:
    """Write MESSAGE to the standard output, framed."""
    body = json.dumps({"jsonrpc": "2.0", **message}).encode()
    sys.stdout.buffer.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    sys.stdout.buffer.flush()


def key(message: dict[str, JSON]) -> str:
    """Return the key of the request MESSAGE in the answers."""
    method = message["method"]
    if method == "workspace/executeCommand":
        return method + " " + message["params"]["command"]
    if method.startswith("callHierarchy/"):
        return method + " " + message["params"]["item"]["data"]["uniqueId"]
    return method


def answer(message: dict[str, JSON], answers: dict[str, JSON]) -> JSON:
    """Return the result of the request MESSAGE."""
    if message["method"] == "initialize":
        experimental = answers.get("$experimental")
        extra = {"experimental": experimental} if experimental else {}
        commands = CAPABILITIES["executeCommandProvider"]["commands"]
        commands = commands + answers.get("$commands", [])
        listed = {"executeCommandProvider": {"commands": commands}}
        added = answers.get("$capabilities", {})
        return {"capabilities": CAPABILITIES | extra | listed | added}
    return answers.get(key(message))


def main() -> None:
    """Serve until `exit` or the end of the input."""
    answers_file = os.environ.get("DBT_MODE_FAKE_ANSWERS")
    answers = json.loads(Path(answers_file).read_text()) if answers_file else {}
    with Path(os.environ.get("DBT_MODE_FAKE_LOG") or os.devnull).open("a") as log:
        log.write(json.dumps({"argv": sys.argv[1:]}) + "\n")
        while (message := read_message(sys.stdin.buffer)) is not None:
            log.write(json.dumps(message) + "\n")
            log.flush()
            method = message.get("method")
            if method == "exit":
                return
            if "id" in message and method is not None:
                time.sleep(answers.get("$delays", {}).get(method, 0))
                error = answers.get("$errors", {}).get(key(message))
                if error is not None:
                    write_message({"id": message["id"], "error": error})
                else:
                    result = answer(message, answers)
                    write_message({"id": message["id"], "result": result})
            if method == "initialized":
                for notification in answers.get("$notifications", NOTIFICATIONS):
                    write_message({"method": notification, "params": {}})


if __name__ == "__main__":
    main()
