#!/usr/bin/env python3
"""
test_dev_tools_config.py
=========================

A standalone diagnostic tool that *ingests* the "Development Tools" (and
"Version Control") sections of an Emacs `config.org` file and verifies that
every external program those Emacs packages rely on is actually installed,
discoverable on ``$PATH``, and (where possible) new enough.

Why this exists
----------------
Packages like ``lsp-mode``, ``dape``, ``apheleia``, ``flycheck`` and
``envrc`` are only as good as the external binaries they shell out to.
Emacs will happily start with a broken toolchain and fail silently (or with
a cryptic error) the first time you actually try to debug, format, or
lint something. This script answers one question up front:

    "If I open Emacs right now, which of my configured dev tools will
    actually work?"

What it does
------------
1. Parses ``config.org`` and extracts every ``(use-package ...)`` declared
   under the ``Development Tools`` / ``Version Control`` headings, plus a
   few concrete details it can pull straight out of the Lisp (git identity,
   tree-sitter grammar sources, and the Apheleia formatter chain for
   Python). This is the "ingestion" step.
2. Cross-references each ingested package against a registry of the
   external binaries it depends on (ripgrep for ``xref``/``grep``, a debug
   adapter for ``dape``, ``direnv`` for ``envrc``, language servers for
   ``lsp-mode``, formatters for ``apheleia``, etc.).
3. Probes the current machine (in parallel) for every one of those
   binaries: is it on ``$PATH``, what version is it, does it clear the
   minimum version this config expects, and -- for a couple of special
   cases -- does the *value* match what the config declares (e.g. does
   ``git config user.email`` match ``user-mail-address`` in config.org)?
4. Renders a color-coded report and can export it as JSON / Markdown /
   HTML for CI or a status dashboard.

Usage
-----
    python3 test_dev_tools_config.py                       # human report
    python3 test_dev_tools_config.py --config ~/.emacs.d/config.org
    python3 test_dev_tools_config.py --category "Debug Adapters"
    python3 test_dev_tools_config.py --json report.json
    python3 test_dev_tools_config.py --markdown report.md
    python3 test_dev_tools_config.py --network             # also probe net
    python3 test_dev_tools_config.py --list-packages       # ingestion only

Exit codes
----------
0  every *required* tool was found and, where a minimum version is known,
   satisfies it.
1  at least one required tool is missing or too old.
2  the script could not run at all (missing dependency, bad arguments).

External dependencies
----------------------
click, rich, pydantic, loguru, psutil, distro, packaging, requests,
humanize, platformdirs

    pip install click rich pydantic loguru psutil distro packaging \\
        requests humanize platformdirs
"""

from __future__ import annotations

import json
import platform
import re
import shutil
import socket
import subprocess
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

try:
    import click
    import distro
    import humanize
    import psutil
    import requests
    from loguru import logger
    from packaging.version import InvalidVersion, Version
    from platformdirs import user_cache_dir
    from pydantic import BaseModel, Field
    from rich.console import Console
    from rich.panel import Panel
    from rich.progress import BarColumn, Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
    from rich.table import Table
    from rich import box
except ImportError as exc:  # pragma: no cover - friendly bootstrap message
    sys.stderr.write(
        "\nMissing dependency: {}\n\n"
        "This script leans on a handful of third-party libraries for a "
        "nicer report. Install them with:\n\n"
        "    pip install click rich pydantic loguru psutil distro "
        "packaging requests humanize platformdirs\n\n".format(exc)
    )
    raise SystemExit(2)

console = Console()

# ---------------------------------------------------------------------------
# 1. Static knowledge: which external binaries does each Emacs package need?
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ToolSpec:
    """Everything needed to look for one external program on this machine."""

    key: str
    label: str
    category: str
    command: str
    version_args: tuple[str, ...] = ("--version",)
    version_regex: str = r"(\d+\.\d+(?:\.\d+)?)"
    min_version: Optional[str] = None
    required: bool = False
    description: str = ""
    install_hints: dict[str, str] = field(default_factory=dict)
    # Some "tools" are really a module probed through a generic launcher
    # (e.g. `python3 -c "import debugpy"`). For those, merely finding the
    # launcher on $PATH (python3 always exists) says nothing about whether
    # the module itself is installed -- the probe's exit code is what
    # actually determines presence.
    probe_determines_presence: bool = False

    def hint_for(self, package_manager: str) -> str:
        return self.install_hints.get(package_manager, self.install_hints.get("generic", "see project docs"))


# Registry of every external tool this config could plausibly need,
# grouped to mirror the org-mode headings they were found under.
TOOLS: dict[str, ToolSpec] = {}


def _register(spec: ToolSpec) -> None:
    TOOLS[spec.key] = spec


_register(ToolSpec(
    key="emacs", label="Emacs", category="Core Runtime", command="emacs",
    version_regex=r"GNU Emacs (\d+\.\d+)", min_version="29.1", required=True,
    description="The editor itself. config.org targets Emacs 30/31.",
    install_hints={"pacman": "sudo pacman -S emacs", "apt": "sudo apt install emacs",
                    "dnf": "sudo dnf install emacs", "brew": "brew install emacs"},
))
_register(ToolSpec(
    key="git", label="Git", category="Core Runtime", command="git",
    version_regex=r"git version (\d+\.\d+\.\d+)", min_version="2.30", required=True,
    description="straight.el bootstrap, magit, forge, git-timemachine, vc-handled-backends.",
    install_hints={"pacman": "sudo pacman -S git", "apt": "sudo apt install git",
                    "dnf": "sudo dnf install git", "brew": "brew install git"},
))

# --- Search & navigation (xref-search-program, grep-*, dirvish-fd) --------
_register(ToolSpec(
    key="ripgrep", label="ripgrep (rg)", category="Search & Navigation", command="rg",
    version_regex=r"ripgrep (\d+\.\d+\.\d+)", min_version="13.0", required=True,
    description="Backs xref-search-program, grep-find-template, consult-ripgrep.",
    install_hints={"pacman": "sudo pacman -S ripgrep", "apt": "sudo apt install ripgrep",
                    "dnf": "sudo dnf install ripgrep", "brew": "brew install ripgrep"},
))
_register(ToolSpec(
    key="fd", label="fd", category="Search & Navigation", command="fd",
    version_regex=r"fd (\d+\.\d+\.\d+)", min_version="8.0", required=False,
    description="Powers dirvish-fd-jump and consult-dir-jump-file-command.",
    install_hints={"pacman": "sudo pacman -S fd", "apt": "sudo apt install fd-find",
                    "dnf": "sudo dnf install fd-find", "brew": "brew install fd"},
))

# --- Debug adapters (dape) --------------------------------------------------
_register(ToolSpec(
    key="debugpy", label="debugpy (Python)", category="Debug Adapters", command="python3",
    version_args=("-c", "import debugpy;print(debugpy.__version__)"),
    version_regex=r"([\d.]+)", required=False, probe_determines_presence=True,
    description="dape's Python debug adapter (pip install debugpy).",
    install_hints={"generic": "pip install --user debugpy"},
))
_register(ToolSpec(
    key="delve", label="delve (dlv, Go)", category="Debug Adapters", command="dlv",
    version_regex=r"Version: (\d+\.\d+\.\d+)", required=False,
    description="dape's Go debug adapter.",
    install_hints={"generic": "go install github.com/go-delve/delve/cmd/dlv@latest"},
))
_register(ToolSpec(
    key="lldb", label="lldb / lldb-dap", category="Debug Adapters", command="lldb",
    version_regex=r"lldb version ([\d.]+)", required=False,
    description="dape's C/C++/Rust debug adapter (lldb-dap or codelldb).",
    install_hints={"pacman": "sudo pacman -S lldb", "apt": "sudo apt install lldb",
                    "dnf": "sudo dnf install lldb", "brew": "brew install llvm"},
))
_register(ToolSpec(
    key="gdb", label="gdb", category="Debug Adapters", command="gdb",
    version_regex=r"GNU gdb.*?(\d+\.\d+)", required=False,
    description="Fallback native debugger for dape's dap-gdb-lldb style adapters.",
    install_hints={"pacman": "sudo pacman -S gdb", "apt": "sudo apt install gdb",
                    "dnf": "sudo dnf install gdb", "brew": "brew install gdb"},
))

# --- Formatters (apheleia) --------------------------------------------------
_register(ToolSpec(
    key="ruff", label="ruff", category="Formatters", command="ruff",
    version_regex=r"ruff (\d+\.\d+\.\d+)", required=False,
    description="config.org explicitly maps python(-ts)-mode to (ruff-isort ruff) in apheleia-mode-alist.",
    install_hints={"generic": "pip install --user ruff", "pacman": "sudo pacman -S ruff",
                    "brew": "brew install ruff"},
))
_register(ToolSpec(
    key="prettier", label="prettier", category="Formatters", command="prettier",
    version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="apheleia's default for JS/TS/CSS/HTML/Markdown/YAML.",
    install_hints={"generic": "npm install -g prettier"},
))
_register(ToolSpec(
    key="gofmt", label="gofmt", category="Formatters", command="gofmt",
    version_regex=r"go(\d+\.\d+)", required=False,
    description="Ships with the Go toolchain; used by apheleia for go-mode.",
    install_hints={"pacman": "sudo pacman -S go", "apt": "sudo apt install golang-go",
                    "brew": "brew install go"},
))
_register(ToolSpec(
    key="rustfmt", label="rustfmt", category="Formatters", command="rustfmt",
    version_regex=r"rustfmt (\d+\.\d+\.\d+)", required=False,
    description="Ships with rustup; used by apheleia for rust-mode.",
    install_hints={"generic": "rustup component add rustfmt"},
))
_register(ToolSpec(
    key="shfmt", label="shfmt", category="Formatters", command="shfmt",
    version_regex=r"v?(\d+\.\d+\.\d+)", required=False,
    description="apheleia formatter for sh-mode / bash-ts-mode.",
    install_hints={"pacman": "sudo pacman -S shfmt", "brew": "brew install shfmt",
                    "generic": "go install mvdan.cc/sh/v3/cmd/shfmt@latest"},
))
_register(ToolSpec(
    key="stylua", label="stylua", category="Formatters", command="stylua",
    version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="apheleia formatter for lua-mode, if configured.",
    install_hints={"generic": "cargo install stylua", "brew": "brew install stylua"},
))

# --- Language servers (lsp-mode) -------------------------------------------
_register(ToolSpec(
    key="pyright", label="pyright", category="Language Servers", command="pyright",
    version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's Python server (alternative: pylsp).",
    install_hints={"generic": "npm install -g pyright"},
))
_register(ToolSpec(
    key="ts-language-server", label="typescript-language-server", category="Language Servers",
    command="typescript-language-server", version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's JS/TS server.",
    install_hints={"generic": "npm install -g typescript-language-server typescript"},
))
_register(ToolSpec(
    key="rust-analyzer", label="rust-analyzer", category="Language Servers", command="rust-analyzer",
    version_regex=r"rust-analyzer ([\d.]+)", required=False,
    description="lsp-mode's Rust server.",
    install_hints={"generic": "rustup component add rust-analyzer"},
))
_register(ToolSpec(
    key="gopls", label="gopls", category="Language Servers", command="gopls",
    version_regex=r"v?(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's Go server.",
    install_hints={"generic": "go install golang.org/x/tools/gopls@latest"},
))
_register(ToolSpec(
    key="clangd", label="clangd", category="Language Servers", command="clangd",
    version_regex=r"clangd version (\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's C/C++ server; also useful for treesit c/cpp grammars.",
    install_hints={"pacman": "sudo pacman -S clang", "apt": "sudo apt install clangd",
                    "dnf": "sudo dnf install clang-tools-extra", "brew": "brew install llvm"},
))
_register(ToolSpec(
    key="bash-language-server", label="bash-language-server", category="Language Servers",
    command="bash-language-server", version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's server for sh-mode/bash-ts-mode.",
    install_hints={"generic": "npm install -g bash-language-server"},
))
_register(ToolSpec(
    key="yaml-language-server", label="yaml-language-server", category="Language Servers",
    command="yaml-language-server", version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's YAML server.",
    install_hints={"generic": "npm install -g yaml-language-server"},
))
_register(ToolSpec(
    key="marksman", label="marksman", category="Language Servers", command="marksman",
    version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="Referenced (currently commented out) in the Markdown Mode / Language Server section.",
    install_hints={"pacman": "sudo pacman -S marksman  # or: yay -S marksman-bin",
                    "brew": "brew install marksman"},
))
_register(ToolSpec(
    key="lua-language-server", label="lua-language-server", category="Language Servers",
    command="lua-language-server", version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's Lua server.",
    install_hints={"pacman": "sudo pacman -S lua-language-server", "brew": "brew install lua-language-server"},
))
_register(ToolSpec(
    key="texlab", label="texlab", category="Language Servers", command="texlab",
    version_regex=r"(\d+\.\d+\.\d+)", required=False,
    description="lsp-mode's LaTeX server; pairs with the LaTeX tree-sitter grammar.",
    install_hints={"generic": "cargo install texlab", "brew": "brew install texlab"},
))

# --- Tree-sitter / compilation toolchain -----------------------------------
_register(ToolSpec(
    key="tree-sitter-cli", label="tree-sitter CLI", category="Tree-sitter Toolchain",
    command="tree-sitter", version_regex=r"tree-sitter (\d+\.\d+\.\d+)", required=False,
    description="Optional; Emacs 30's treesit-install-language-grammar compiles grammars itself.",
    install_hints={"generic": "npm install -g tree-sitter-cli", "brew": "brew install tree-sitter"},
))
_register(ToolSpec(
    key="cc", label="C compiler (cc)", category="Tree-sitter Toolchain", command="cc",
    version_regex=r"(\d+\.\d+(?:\.\d+)?)", required=True,
    description="Required by treesit-auto-install-grammar to compile every grammar in config.org.",
    install_hints={"pacman": "sudo pacman -S base-devel", "apt": "sudo apt install build-essential",
                    "dnf": "sudo dnf groupinstall 'Development Tools'", "brew": "xcode-select --install"},
))
_register(ToolSpec(
    key="make", label="make", category="Tree-sitter Toolchain", command="make",
    version_regex=r"GNU Make (\d+\.\d+)", required=False,
    description="Used by projection's Makefile project type and some grammar builds.",
    install_hints={"pacman": "sudo pacman -S make", "apt": "sudo apt install make",
                    "dnf": "sudo dnf install make", "brew": "brew install make"},
))

# --- Environment isolation --------------------------------------------------
_register(ToolSpec(
    key="direnv", label="direnv", category="Environment Isolation", command="direnv",
    version_regex=r"(\d+\.\d+\.\d+)", min_version="2.30", required=True,
    description="envrc-global-mode shells out to direnv for every project buffer.",
    install_hints={"pacman": "sudo pacman -S direnv", "apt": "sudo apt install direnv",
                    "dnf": "sudo dnf install direnv", "brew": "brew install direnv"},
))

# --- Session / compression (undo-fu-session) -------------------------------
_register(ToolSpec(
    key="zstd", label="zstd", category="Session & Compression", command="zstd",
    version_regex=r"v(\d+\.\d+\.\d+)", required=False,
    description="undo-fu-session-compression prefers zstd for near-instant I/O.",
    install_hints={"pacman": "sudo pacman -S zstd", "apt": "sudo apt install zstd",
                    "dnf": "sudo dnf install zstd", "brew": "brew install zstd"},
))
_register(ToolSpec(
    key="gzip", label="gzip", category="Session & Compression", command="gzip",
    version_regex=r"(\d+\.\d+)", required=False,
    description="undo-fu-session's fallback compression if zstd is unavailable.",
    install_hints={"generic": "gzip ships with virtually every OS by default"},
))

# --- Documentation -----------------------------------------------------------
_register(ToolSpec(
    key="man", label="man", category="Documentation", command="man",
    version_args=("--version",), version_regex=r"(\d+\.\d+(?:\.\d+)?)", required=False,
    description="woman extracts `manpath -q` / `man --path` for manual lookup.",
    install_hints={"pacman": "sudo pacman -S man-db", "apt": "sudo apt install man-db",
                    "dnf": "sudo dnf install man-db", "brew": "man ships with macOS by default"},
))
_register(ToolSpec(
    key="python3", label="Python 3", category="Documentation", command="python3",
    version_regex=r"Python (\d+\.\d+\.\d+)", min_version="3.8", required=False,
    description="Used by treemacs-git-mode 'deferred (async git status parsing) and debugpy.",
    install_hints={"pacman": "sudo pacman -S python", "apt": "sudo apt install python3",
                    "dnf": "sudo dnf install python3", "brew": "brew install python3"},
))
_register(ToolSpec(
    key="gpg", label="gpg", category="Documentation", command="gpg",
    version_regex=r"gpg \(GnuPG\) (\d+\.\d+\.\d+)", required=False,
    description="epg-pinentry-mode 'loopback routes GPG prompts through gpg.",
    install_hints={"pacman": "sudo pacman -S gnupg", "apt": "sudo apt install gnupg",
                    "dnf": "sudo dnf install gnupg2", "brew": "brew install gnupg"},
))


# Maps an ingested `(use-package NAME ...)` symbol to the tool keys (above)
# it depends on. Packages not present here are still reported, just with
# an empty dependency list ("pure-Emacs-Lisp, nothing external to check").
PACKAGE_TOOL_DEPENDENCIES: dict[str, list[str]] = {
    "eldoc": [],
    "xref": ["ripgrep"],
    "grep": ["ripgrep"],
    "lsp-mode": ["pyright", "ts-language-server", "rust-analyzer", "gopls",
                 "clangd", "bash-language-server", "yaml-language-server"],
    "lsp-ui": [],
    "consult-lsp": [],
    "lsp-treemacs": [],
    "flycheck": [],
    "consult-flycheck": [],
    "demap": [],
    "dape": ["debugpy", "delve", "lldb", "gdb"],
    "envrc": ["direnv"],
    "apheleia": ["ruff", "prettier", "gofmt", "rustfmt", "shfmt", "stylua"],
    "magit": ["git"],
    "forge": ["git"],
    "diff-hl": [],
    "git-timemachine": ["git"],
    "treesit": ["tree-sitter-cli", "cc"],
    "undo-fu-session": ["zstd", "gzip"],
    "woman": ["man"],
    "dirvish": ["fd"],
    "consult-dir": ["fd"],
    "projection": ["make"],
    "markdown-ts-mode": ["marksman"],
}


# ---------------------------------------------------------------------------
# 2. Ingestion: pull declared dev-tool packages straight out of config.org
# ---------------------------------------------------------------------------

TOP_HEADING_RE = re.compile(r"^\*\s+(?:(?:TODO|DONE)\s+)?(.+?)\s*$")
USE_PACKAGE_RE = re.compile(r"\(use-package\s+([A-Za-z0-9_/+\-]+)")
IDENTITY_NAME_RE = re.compile(r'user-full-name\s+"([^"]+)"')
IDENTITY_MAIL_RE = re.compile(r'user-mail-address\s+"([^"]+)"')
TREESIT_SOURCE_RE = re.compile(r'\(([a-zA-Z0-9_\-]+)\s+"(https://\S+?)"\)')
APHELEIA_MODE_RE = re.compile(
    r"alist-get\s+'([a-zA-Z0-9_\-]+)\s+apheleia-mode-alist\)\s+'\(([^)]*)\)"
)

DEFAULT_SECTIONS = ("Development Tools", "Version Control")


class ConfigPackage(BaseModel):
    name: str
    heading: str
    line: int


class IngestedConfig(BaseModel):
    path: Optional[str] = None
    found: bool = False
    packages: list[ConfigPackage] = Field(default_factory=list)
    git_full_name: Optional[str] = None
    git_mail_address: Optional[str] = None
    treesit_grammars: dict[str, str] = Field(default_factory=dict)
    apheleia_formatters: dict[str, str] = Field(default_factory=dict)


def ingest_config(path: Optional[Path], sections: tuple[str, ...]) -> IngestedConfig:
    """Parse config.org (if we can find/read it) and extract dev-tool facts.

    This never raises: if the file is missing we simply fall back to the
    static registry above (which was itself hand-ingested from the same
    config.org when this script was written), so the tool always works.
    """
    if path is None:
        for candidate in (
            Path("config.org"),
            Path.home() / ".emacs.d" / "config.org",
            Path.home() / ".config" / "emacs" / "config.org",
        ):
            if candidate.exists():
                path = candidate
                break

    result = IngestedConfig(path=str(path) if path else None)
    if path is None or not path.exists():
        return result

    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError as exc:
        logger.warning("Could not read {}: {}", path, exc)
        return result

    result.found = True
    current_heading = ""
    in_target_section = False
    for lineno, line in enumerate(text.splitlines(), start=1):
        top_match = TOP_HEADING_RE.match(line)
        if top_match:
            current_heading = top_match.group(1)
            in_target_section = any(s.lower() in current_heading.lower() for s in sections)
            continue
        if in_target_section:
            pkg_match = USE_PACKAGE_RE.search(line)
            if pkg_match:
                result.packages.append(
                    ConfigPackage(name=pkg_match.group(1), heading=current_heading, line=lineno)
                )

    name_match = IDENTITY_NAME_RE.search(text)
    mail_match = IDENTITY_MAIL_RE.search(text)
    result.git_full_name = name_match.group(1) if name_match else None
    result.git_mail_address = mail_match.group(1) if mail_match else None

    # Dynamically discover tree-sitter grammar sources so the report
    # reflects *this* config.org rather than a hardcoded snapshot. The
    # `(dolist (source '((lang "url") ...))` block precedes the line that
    # actually mentions `treesit-language-source-alist`, so anchor on the
    # dolist itself and scan forward through the list.
    treesit_block_start = text.find("dolist (source")
    if treesit_block_start != -1:
        window = text[treesit_block_start: treesit_block_start + 3000]
        for lang, url in TREESIT_SOURCE_RE.findall(window):
            result.treesit_grammars[lang] = url

    for mode, chain in APHELEIA_MODE_RE.findall(text):
        result.apheleia_formatters[mode] = chain.strip()

    return result


# ---------------------------------------------------------------------------
# 3. Probing the machine
# ---------------------------------------------------------------------------


class ToolResult(BaseModel):
    key: str
    label: str
    category: str
    required: bool
    found: bool
    path: Optional[str] = None
    version: Optional[str] = None
    min_version: Optional[str] = None
    version_ok: Optional[bool] = None
    description: str = ""
    install_hint: str = ""
    source_packages: list[str] = Field(default_factory=list)
    error: Optional[str] = None


def _detect_package_manager() -> str:
    system = platform.system().lower()
    if system == "darwin":
        return "brew"
    if system == "linux":
        try:
            dist_id = distro.id().lower()
        except Exception:  # pragma: no cover - distro should never really throw
            dist_id = ""
        if dist_id in {"arch", "manjaro", "endeavouros"}:
            return "pacman"
        if dist_id in {"fedora", "rhel", "centos"}:
            return "dnf"
        if dist_id in {"ubuntu", "debian", "pop", "linuxmint"}:
            return "apt"
    return "generic"


def probe_tool(spec: ToolSpec) -> ToolResult:
    path = shutil.which(spec.command)
    result = ToolResult(
        key=spec.key, label=spec.label, category=spec.category, required=spec.required,
        found=path is not None, path=path, min_version=spec.min_version,
        description=spec.description,
    )
    if path is None:
        return result

    try:
        proc = subprocess.run(
            [path, *spec.version_args], capture_output=True, text=True, timeout=6,
        )
        if spec.probe_determines_presence and proc.returncode != 0:
            # The launcher exists, but whatever we probed for (a module,
            # a subcommand, ...) does not. Don't report a false positive.
            result.found = False
            result.path = None
            stderr_tail = (proc.stderr or "").strip().splitlines()
            result.error = stderr_tail[-1][:120] if stderr_tail else "probe exited non-zero"
            return result
        raw_output = (proc.stdout or "") + (proc.stderr or "")
        match = re.search(spec.version_regex, raw_output)
        if match:
            result.version = match.group(1)
        elif raw_output.strip():
            result.version = raw_output.strip().splitlines()[0][:60]
    except (subprocess.TimeoutExpired, OSError) as exc:
        result.error = f"version probe failed: {exc}"

    if result.version and spec.min_version:
        try:
            result.version_ok = Version(result.version) >= Version(spec.min_version)
        except InvalidVersion:
            result.version_ok = None

    return result


def probe_all(specs: list[ToolSpec]) -> list[ToolResult]:
    results: list[ToolResult] = []
    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TextColumn("{task.completed}/{task.total}"),
        TimeElapsedColumn(),
        console=console,
        transient=True,
    ) as progress:
        task = progress.add_task("Probing external dev tools...", total=len(specs))
        with __import__("concurrent.futures", fromlist=["ThreadPoolExecutor"]).ThreadPoolExecutor(
            max_workers=min(12, max(4, len(specs)))
        ) as pool:
            futures = {pool.submit(probe_tool, spec): spec for spec in specs}
            for future in __import__("concurrent.futures", fromlist=["as_completed"]).as_completed(futures):
                results.append(future.result())
                progress.advance(task)
    order = {spec.key: i for i, spec in enumerate(specs)}
    results.sort(key=lambda r: order.get(r.key, 999))
    return results


# ---------------------------------------------------------------------------
# 4. System context (rounds out the report; also demonstrates psutil/distro)
# ---------------------------------------------------------------------------


class SystemInfo(BaseModel):
    hostname: str
    os_name: str
    os_version: str
    kernel: str
    python_version: str
    cpu_count: Optional[int]
    total_memory: str
    package_manager: str
    emacs_running: bool
    emacs_pids: list[int] = Field(default_factory=list)


def gather_system_info() -> SystemInfo:
    try:
        dist_pretty = distro.name(pretty=True) or platform.platform()
    except Exception:
        dist_pretty = platform.platform()

    emacs_pids = []
    try:
        for proc in psutil.process_iter(attrs=["pid", "name"]):
            name = (proc.info.get("name") or "").lower()
            if name.startswith("emacs"):
                emacs_pids.append(proc.info["pid"])
    except Exception:  # pragma: no cover - psutil is best-effort here
        pass

    try:
        total_mem = humanize.naturalsize(psutil.virtual_memory().total, binary=True)
    except Exception:
        total_mem = "unknown"

    return SystemInfo(
        hostname=socket.gethostname(),
        os_name=platform.system(),
        os_version=dist_pretty,
        kernel=platform.release(),
        python_version=platform.python_version(),
        cpu_count=psutil.cpu_count(logical=True),
        total_memory=total_mem,
        package_manager=_detect_package_manager(),
        emacs_running=bool(emacs_pids),
        emacs_pids=emacs_pids,
    )


# ---------------------------------------------------------------------------
# 5. Reporting
# ---------------------------------------------------------------------------


class DiagnosticReport(BaseModel):
    generated_at: str
    config: IngestedConfig
    system: SystemInfo
    results: list[ToolResult]

    @property
    def required_missing(self) -> list[ToolResult]:
        return [r for r in self.results if r.required and not r.found]

    @property
    def required_outdated(self) -> list[ToolResult]:
        return [r for r in self.results if r.required and r.found and r.version_ok is False]

    @property
    def ok(self) -> bool:
        return not self.required_missing and not self.required_outdated


def status_cell(result: ToolResult) -> str:
    if not result.found:
        return "[bold red]✗ missing[/]" if result.required else "[yellow]— not found (optional)[/]"
    if result.version_ok is False:
        return "[bold yellow]⚠ outdated[/]"
    if result.version_ok is True:
        return "[bold green]✓ ok[/]"
    return "[green]✓ found[/]"


def render_config_summary(cfg: IngestedConfig, console_: Console) -> None:
    if not cfg.found:
        console_.print(Panel(
            "No config.org found locally -- falling back to the built-in registry that was "
            "hand-ingested from the reference configuration. Pass --config /path/to/config.org "
            "to ingest a live copy.",
            title="Ingestion", border_style="yellow",
        ))
        return

    table = Table(title=f"Ingested from {cfg.path}", box=box.SIMPLE_HEAVY, show_lines=False)
    table.add_column("Package", style="cyan", no_wrap=True)
    table.add_column("Heading", style="magenta")
    table.add_column("Line", justify="right", style="dim")
    for pkg in cfg.packages:
        table.add_row(pkg.name, pkg.heading, str(pkg.line))
    console_.print(table)

    if cfg.git_full_name or cfg.git_mail_address:
        try:
            git_name = subprocess.run(
                ["git", "config", "--global", "user.name"], capture_output=True, text=True, timeout=3
            ).stdout.strip() or None
            git_mail = subprocess.run(
                ["git", "config", "--global", "user.email"], capture_output=True, text=True, timeout=3
            ).stdout.strip() or None
        except (OSError, subprocess.TimeoutExpired):
            git_name = git_mail = None

        identity_table = Table(title="Git identity vs. config.org", box=box.SIMPLE_HEAVY)
        identity_table.add_column("Field")
        identity_table.add_column("config.org")
        identity_table.add_column("git config --global")
        identity_table.add_column("Match?")
        for field_label, cfg_val, git_val in (
            ("Name", cfg.git_full_name, git_name),
            ("Email", cfg.git_mail_address, git_mail),
        ):
            if cfg_val is None:
                continue
            match = "[green]yes[/]" if git_val == cfg_val else "[yellow]no[/]"
            identity_table.add_row(field_label, cfg_val or "-", git_val or "[dim]not set[/]", match)
        console_.print(identity_table)

    if cfg.treesit_grammars:
        console_.print(Panel(
            "Tree-sitter grammars declared in treesit-language-source-alist: "
            + ", ".join(sorted(cfg.treesit_grammars)),
            title="Tree-sitter sources (ingested)", border_style="blue",
        ))

    if cfg.apheleia_formatters:
        lines = [f"{mode} -> {chain}" for mode, chain in cfg.apheleia_formatters.items()]
        console_.print(Panel("\n".join(lines), title="Apheleia formatter chains (ingested)",
                              border_style="blue"))


def render_results_table(results: list[ToolResult], console_: Console) -> None:
    by_category: dict[str, list[ToolResult]] = {}
    for r in results:
        by_category.setdefault(r.category, []).append(r)

    for category, items in by_category.items():
        table = Table(title=category, box=box.ROUNDED, show_lines=False)
        table.add_column("Tool")
        table.add_column("Status")
        table.add_column("Version")
        table.add_column("Min.")
        table.add_column("Path", overflow="fold")
        table.add_column("Used by")
        for r in items:
            path_cell = r.path or f"[dim]{TOOLS[r.key].hint_for('generic') if r.key in TOOLS else ''}[/]"
            if not r.found and r.error:
                path_cell = f"[dim]{path_cell}\n({r.error})[/dim]"
            table.add_row(
                r.label + (" [red]*[/]" if r.required else ""),
                status_cell(r),
                r.version or "-",
                r.min_version or "-",
                path_cell,
                ", ".join(r.source_packages) or "[dim]-[/]",
            )
        console_.print(table)
    console_.print("[dim]* required tool[/dim]\n")


def render_summary_panel(report: DiagnosticReport, console_: Console) -> None:
    total = len(report.results)
    found = sum(1 for r in report.results if r.found)
    required_total = sum(1 for r in report.results if r.required)
    required_found = sum(1 for r in report.results if r.required and r.found)

    lines = [
        f"Host: {report.system.hostname}  |  OS: {report.system.os_version}  |  "
        f"Python: {report.system.python_version}  |  CPUs: {report.system.cpu_count}  |  "
        f"RAM: {report.system.total_memory}",
        f"Emacs currently running: "
        + ("[green]yes[/] (pid " + ", ".join(map(str, report.system.emacs_pids)) + ")"
           if report.system.emacs_running else "[yellow]no[/]"),
        "",
        f"Tools found: {found}/{total}   |   Required tools satisfied: {required_found}/{required_total}",
    ]
    style = "green" if report.ok else "red"
    title = "✅ All required development tools are ready" if report.ok else "❌ Action needed"
    console_.print(Panel("\n".join(lines), title=title, border_style=style))

    if report.required_missing:
        console_.print("[bold red]Missing required tools:[/bold red]")
        for r in report.required_missing:
            hint = TOOLS[r.key].hint_for(report.system.package_manager) if r.key in TOOLS else ""
            console_.print(f"  • {r.label}: {hint}")
    if report.required_outdated:
        console_.print("[bold yellow]Outdated required tools:[/bold yellow]")
        for r in report.required_outdated:
            console_.print(f"  • {r.label}: found {r.version}, need >= {r.min_version}")


def maybe_probe_network(console_: Console) -> None:
    console_.print("[dim]Checking network reachability (straight.el bootstrap / package registries)...[/dim]")
    endpoints = {
        "github.com (straight.el, most packages)": "https://github.com",
        "radian-software.github.io (straight.el bootstrap)": "https://radian-software.github.io",
        "pypi.org (pip installs: debugpy, ruff, pyright)": "https://pypi.org",
    }
    table = Table(box=box.SIMPLE_HEAVY)
    table.add_column("Endpoint")
    table.add_column("Reachable")
    table.add_column("Latency")
    for label, url in endpoints.items():
        start = time.perf_counter()
        try:
            requests.head(url, timeout=4, allow_redirects=True)
            elapsed = f"{(time.perf_counter() - start) * 1000:.0f} ms"
            table.add_row(label, "[green]yes[/]", elapsed)
        except requests.RequestException as exc:
            table.add_row(label, f"[red]no[/] ({exc.__class__.__name__})", "-")
    console_.print(table)


# ---------------------------------------------------------------------------
# 6. Exporters
# ---------------------------------------------------------------------------


def export_json(report: DiagnosticReport, out_path: Path) -> None:
    out_path.write_text(report.model_dump_json(indent=2), encoding="utf-8")


def export_markdown(report: DiagnosticReport, out_path: Path) -> None:
    lines = [f"# Dev Tools Report ({report.generated_at})", ""]
    lines.append(f"- Host: `{report.system.hostname}` ({report.system.os_version})")
    lines.append(f"- Overall status: {'✅ OK' if report.ok else '❌ Needs attention'}")
    lines.append("")
    by_category: dict[str, list[ToolResult]] = {}
    for r in report.results:
        by_category.setdefault(r.category, []).append(r)
    for category, items in by_category.items():
        lines.append(f"## {category}")
        lines.append("")
        lines.append("| Tool | Status | Version | Min | Used by |")
        lines.append("|---|---|---|---|---|")
        for r in items:
            status = "✗ missing" if not r.found else ("⚠ outdated" if r.version_ok is False else "✓ ok")
            lines.append(
                f"| {r.label}{' *' if r.required else ''} | {status} | {r.version or '-'} | "
                f"{r.min_version or '-'} | {', '.join(r.source_packages) or '-'} |"
            )
        lines.append("")
    out_path.write_text("\n".join(lines), encoding="utf-8")


def export_html(report: DiagnosticReport, out_path: Path) -> None:
    console_ = Console(record=True, width=140)
    render_config_summary(report.config, console_)
    render_results_table(report.results, console_)
    render_summary_panel(report, console_)
    out_path.write_text(console_.export_html(inline_styles=True), encoding="utf-8")


# ---------------------------------------------------------------------------
# 7. CLI
# ---------------------------------------------------------------------------


@click.command(context_settings={"help_option_names": ["-h", "--help"]})
@click.option("--config", "config_path", type=click.Path(path_type=Path), default=None,
              help="Path to your Emacs config.org (default: auto-detect common locations).")
@click.option("--sections", multiple=True, default=DEFAULT_SECTIONS, show_default=True,
              help="Org headings to ingest packages from (case-insensitive substring match).")
@click.option("--category", "categories", multiple=True,
              help="Only show these report categories (repeatable). Default: all.")
@click.option("--network/--no-network", default=False,
              help="Also probe network reachability of key endpoints (github, pypi, ...).")
@click.option("--json", "json_out", type=click.Path(path_type=Path), default=None,
              help="Write the full report as JSON to this path.")
@click.option("--markdown", "markdown_out", type=click.Path(path_type=Path), default=None,
              help="Write a Markdown summary to this path.")
@click.option("--html", "html_out", type=click.Path(path_type=Path), default=None,
              help="Write a styled HTML report to this path.")
@click.option("--list-packages", is_flag=True,
              help="Only run the ingestion step and print discovered packages, then exit.")
@click.option("--quiet", is_flag=True, help="Suppress the human-readable console report.")
@click.option("--log-file", type=click.Path(path_type=Path), default=None,
              help="Also write a debug log here (default: platform cache dir).")
def main(
    config_path: Optional[Path],
    sections: tuple[str, ...],
    categories: tuple[str, ...],
    network: bool,
    json_out: Optional[Path],
    markdown_out: Optional[Path],
    html_out: Optional[Path],
    list_packages: bool,
    quiet: bool,
    log_file: Optional[Path],
) -> None:
    """Ingest an Emacs config.org and verify its development tools are installed."""
    logger.remove()
    cache_dir = Path(user_cache_dir("test-dev-tools-config"))
    cache_dir.mkdir(parents=True, exist_ok=True)
    log_path = log_file or (cache_dir / "run.log")
    logger.add(log_path, rotation="1 MB", retention=5, level="DEBUG")
    logger.info("Starting dev-tools diagnostic run")

    cfg = ingest_config(config_path, sections)
    logger.info("Ingested {} package(s) from {}", len(cfg.packages), cfg.path or "<built-in registry>")

    if list_packages:
        render_config_summary(cfg, console)
        raise SystemExit(0)

    # Build source_packages back-references: tool_key -> [elisp package names]
    tool_to_packages: dict[str, list[str]] = {}
    ingested_names = {p.name for p in cfg.packages} if cfg.found else set(PACKAGE_TOOL_DEPENDENCIES)
    relevant_packages = ingested_names or set(PACKAGE_TOOL_DEPENDENCIES)
    for pkg_name in relevant_packages:
        for tool_key in PACKAGE_TOOL_DEPENDENCIES.get(pkg_name, []):
            tool_to_packages.setdefault(tool_key, []).append(pkg_name)

    specs_to_probe = [spec for spec in TOOLS.values()
                       if spec.key in tool_to_packages or spec.category == "Core Runtime"]
    if categories:
        wanted = {c.lower() for c in categories}
        specs_to_probe = [s for s in specs_to_probe if s.category.lower() in wanted]

    if not quiet:
        console.print(Panel.fit(
            "[bold]Emacs Development Tools Diagnostic[/bold]\n"
            f"Ingesting: {', '.join(sections)}",
            border_style="cyan",
        ))
        render_config_summary(cfg, console)

    results = probe_all(specs_to_probe)
    for r in results:
        r.source_packages = tool_to_packages.get(r.key, [])
        if r.key in TOOLS:
            r.install_hint = TOOLS[r.key].hint_for(_detect_package_manager())

    system = gather_system_info()
    report = DiagnosticReport(
        generated_at=datetime.now(timezone.utc).isoformat(timespec="seconds"),
        config=cfg, system=system, results=results,
    )

    if not quiet:
        render_results_table(results, console)
        render_summary_panel(report, console)
        if network:
            maybe_probe_network(console)

    if json_out:
        export_json(report, json_out)
        console.print(f"[dim]JSON report written to {json_out}[/dim]")
    if markdown_out:
        export_markdown(report, markdown_out)
        console.print(f"[dim]Markdown report written to {markdown_out}[/dim]")
    if html_out:
        export_html(report, html_out)
        console.print(f"[dim]HTML report written to {html_out}[/dim]")

    logger.info("Run complete. ok={} required_missing={} required_outdated={}",
                report.ok, len(report.required_missing), len(report.required_outdated))

    raise SystemExit(0 if report.ok else 1)


if __name__ == "__main__":
    main()
