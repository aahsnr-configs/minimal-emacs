---
title: "Execute Command"
category: "Code Actions & Refactoring"
status: "Working"
parity: "Server-defined custom commands surfaced in the Command Palette (e.g., \"Restart Server\", \"Expand Macro\")"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Execute Command</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Code Actions &amp; Refactoring</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Server-defined custom commands surfaced in the Command Palette
            (e.g., "Restart Server", "Expand Macro")</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>workspace/executeCommand</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-execute-command<span class="route-arrow">→</span>completing-read (Vertico/Consult)</code>
</div>
</div>
</header>

<article class="acc">
<button aria-controls="sect-overview" aria-expanded="true" class="acc-head open">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="7" width="7" x="3" y="3"></rect>
<rect height="7" width="7" x="14" y="3"></rect>
<rect height="7" width="7" x="14" y="14"></rect>
<rect height="7" width="7" x="3" y="14"></rect>
</svg>
            Feature Overview
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body open" id="sect-overview" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Behavioral Parity Matrix</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>VS Code Behavior</th>
<th>Emacs 31 Equivalent</th>
</tr>
</thead>
<tbody>
<tr>
<td>Command Palette shows server commands</td>
<td>
<kbd>SPC c x</kbd> (<code>eglot-execute-command</code>)
                        lists commands via <code>completing-read</code>.
                      </td>
</tr>
<tr>
<td>"Organize Imports" quick action</td>
<td>
<kbd>SPC c i</kbd>
                        (<code>eglot-code-action-organize-imports</code>) or
                        <kbd>SPC c a</kbd> (Code Actions).
                      </td>
</tr>
<tr>
<td>Command applies workspace edits</td>
<td>
<code>eglot</code> natively processes the
                        <code>WorkspaceEdit</code> response and updates affected
                        buffers atomically.
                      </td>
</tr>
<tr>
<td>Fails gracefully if unsupported</td>
<td>
<code>eglot</code> validates
                        <code>executeCommandProvider</code> capabilities before
                        offering the command.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-ecosystem" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M2 12h20M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"></path>
</svg>
            Ecosystem Integration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-ecosystem" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div>
<div class="eco-name">eglot</div>
<div class="eco-sub">LSP Client</div>
</div>
</div>
<p class="eco-desc">
                    Queries the server's <code>executeCommandProvider</code>
                    capabilities during initialization and routes the
                    <code>workspace/executeCommand</code> JSON-RPC request.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div>
<div class="eco-name">vertico / consult</div>
<div class="eco-sub">UI Engine</div>
</div>
</div>
<p class="eco-desc">
                    Presents the list of available server-defined commands as a
                    searchable, fuzzy-filtered dropdown, inheriting live
                    previews and keyboard navigation.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">apheleia</div>
<div class="eco-sub">Formatting Synergy</div>
</div>
</div>
<p class="eco-desc">
                    For Python, formatting &amp; import sorting is handled
                    asynchronously on save by <code>apheleia</code> (running
                    <code>ruff</code>), reserving <code>executeCommand</code>
                    for deep AST-aware semantic refactoring.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-stack" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
            Implementation Stack
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-stack" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="stack-card">
<div class="stack-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot</div>
<div class="stack-role">LSP Client</div>
<div class="stack-desc">
                      Built-in. Routes the <code>workspace/executeCommand</code>
                      JSON-RPC request and validates server capabilities.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">completing-read / vertico</div>
<div class="stack-role">UI Engine</div>
<div class="stack-desc">
                      Presents the list of available server-defined commands as
                      a searchable, fuzzy-filtered dropdown.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot--apply-workspace-edit</div>
<div class="stack-role">Workspace Edit Handler</div>
<div class="stack-desc">
                      Natively processes any <code>WorkspaceEdit</code> payloads
                      returned by the executed command (e.g., multi-file
                      refactoring).
                    </div>
</div>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-commands" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
            Commands &amp; Keybindings
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-commands" role="region">
<div>
<div class="acc-inner">
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Action</th>
<th>Command</th>
<th>Keybinding</th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr>
<td>Execute server command</td>
<td><code>eglot-execute-command</code></td>
<td><kbd>SPC c x</kbd></td>
<td>
                        Prompts for a command ID registered by the active LSP
                        server.
                      </td>
</tr>
<tr>
<td>Organize imports (LSP)</td>
<td><code>eglot-code-action-organize-imports</code></td>
<td><kbd>SPC c i</kbd></td>
<td>
                        Built-in wrapper for the standard LSP organize imports
                        command.
                      </td>
</tr>
<tr>
<td>Code actions menu</td>
<td><code>eglot-code-actions</code></td>
<td><kbd>SPC c a</kbd></td>
<td>
                        Often surfaces commands like "Fix All" as actionable
                        items, bypassing the need to type raw command IDs.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-config" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="16 18 22 12 16 6"></polyline>
<polyline points="8 6 2 12 8 18"></polyline>
</svg>
            Configuration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-config" role="region">
<div>
<div class="acc-inner">
<div class="code-win">
<div class="code-head">
<div style="display: flex; align-items: center">
<div aria-hidden="true" class="dots">
<span></span><span></span><span></span>
</div>
<span class="fname">init-execute-command.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
{% raw %}<pre><code class="language-lisp">;; ==========================================
;; EGLOT EXECUTE COMMAND (Built-in)
;; ==========================================
;; eglot natively provides `eglot-execute-command` to surface
;; server-defined commands via `completing-read`.
;; No explicit package configuration is required beyond standard eglot setup.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c x" '(eglot-execute-command :wk "Execute server command")
  "c i" '(eglot-code-action-organize-imports :wk "Organize imports (LSP)"))</code></pre>{% endraw %}
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-arch" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"></path>
</svg>
            Architecture &amp; Enhancements
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-arch" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Why This Approach?</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="vs-card ok">
<h4>✓ eglot native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works exclusively with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Leverages the existing <code>vertico</code> /
                        <code>consult</code> stack for instant, fuzzy-filtered
                        command selection.</span>
</div>
<div class="vs-row">
<span class="lab">Safety</span>
<span class="val"><code>eglot</code> checks server capabilities before
                        prompting, hiding unsupported commands.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / Custom Wrappers · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Uses heavy, custom child-frame pipelines or bespoke
                        completion UIs.</span>
</div>
<div class="vs-row">
<span class="lab">Safety</span>
<span class="val">Often requires manual filtering or risks executing
                        unsupported commands.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Refined WorkspaceEdit Handling</div>
<p class="enh-desc">
                    Emacs 31's <code>eglot</code> robustly handles complex
                    <code>WorkspaceEdit</code> payloads returned by
                    <code>executeCommand</code>, including multi-file edits,
                    file creations, and deletions, applying them atomically
                    without corrupting undo history.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Seamless consult Integration</div>
<p class="enh-desc">
                    Because <code>eglot-execute-command</code> relies on the
                    native <code>completing-read</code> API, it automatically
                    inherits the fuzzy filtering, live previews, and keyboard
                    navigation provided by the <code>vertico</code> +
                    <code>consult</code> stack.
                  </p>
</div>
</div>
<div class="sec-title">Integration Context (Python)</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Task</th>
<th>Tool</th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr>
<td>Formatting &amp; Import Sorting</td>
<td><code>apheleia</code> (<code>ruff</code>)</td>
<td>
                        Handled asynchronously on save. Faster and more reliable
                        than asking the LSP server.
                      </td>
</tr>
<tr>
<td>Semantic Refactoring</td>
<td>
<code>eglot-execute-command</code> /
                        <code>eglot-code-actions</code>
</td>
<td>
                        Reserved for deep AST-aware operations (e.g., "Extract
                        to variable", "Convert to f-string").
                      </td>
</tr>
</tbody>
</table>
</div>
<div class="sec-title">Troubleshooting</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Issue</th>
<th>Cause &amp; Solution</th>
</tr>
</thead>
<tbody>
<tr>
<td>Command Not Found or Fails</td>
<td>
                        Verify Server Support: Not all language servers expose
                        custom commands. Run
                        <kbd>M-x eglot-describe-connection</kbd> to check if
                        <code>executeCommandProvider</code> is advertised. Use
                        Code Actions Instead: Many servers bundle "Organize
                        Imports" into <code>textDocument/codeAction</code> (<kbd>SPC c a</kbd>) rather than <code>workspace/executeCommand</code>.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

