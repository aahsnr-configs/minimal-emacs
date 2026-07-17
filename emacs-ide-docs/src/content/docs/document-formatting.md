---
title: "Document Formatting (Whole File)"
category: "Formatting & Editing"
status: "Working"
parity: "\"Format Document\" command (Shift+Alt+F)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Document Formatting (Whole File)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Formatting &amp; Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>"Format Document" command (Shift+Alt+F)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/formatting</code>
<span aria-hidden="true" class="meta-sep">·</span>
<span style="color: var(--text-dim); font-size: 12px">(bypassed for whole-file ops)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>apheleia (async CLI diff) OR eglot<span class="route-arrow">→</span>eglot-format-buffer</code>
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
<td><kbd>Shift+Alt+F</kbd> formats the whole file</td>
<td>
<kbd>SPC c f</kbd>
                        (<code>apheleia-format-buffer</code>) or
                        <kbd>C-c C-f</kbd> (<code>eglot-format-buffer</code>).
                      </td>
</tr>
<tr>
<td>Format on Save toggle</td>
<td>
<code>apheleia-global-mode +1</code> hooks into
                        <code>before-save-hook</code>.
                      </td>
</tr>
<tr>
<td>No UI freeze during formatting</td>
<td>
<code>apheleia</code> runs asynchronously; Emacs remains
                        fully interactive.
                      </td>
</tr>
<tr>
<td>Cursor stays in place after format</td>
<td>
<code>apheleia</code>'s diff engine precisely restores
                        the cursor and mark.
                      </td>
</tr>
<tr>
<td>Undo history remains clean</td>
<td>
<code>apheleia</code> groups the diff application into a
                        single, clean undo step.
                      </td>
</tr>
<tr>
<td>Project-specific formatter rules</td>
<td>
<code>apheleia</code> respects
                        <code>.dir-locals.el</code> and
                        <code>project.el</code> root detection.
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
<div class="eco-sub">Semantic Actions</div>
</div>
</div>
<p class="eco-desc">
                    Handles all semantic code actions (like "Organize Imports"
                    or "Extract Method"), while <code>apheleia</code> handles
                    the syntactic whole-file formatting.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
</div>
<div>
<div class="eco-name">project.el</div>
<div class="eco-sub">Project Resolution</div>
</div>
</div>
<p class="eco-desc">
<code>apheleia</code> uses <code>project-root</code> to
                    locate configuration files (e.g., <code>.prettierrc</code>,
                    <code>pyproject.toml</code>), ensuring the formatter runs
                    with correct project rules.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div>
<div class="eco-name">magit / vc</div>
<div class="eco-sub">Version Control</div>
</div>
</div>
<p class="eco-desc">
                    Because <code>apheleia</code> runs on
                    <code>before-save-hook</code>, files are always cleanly
                    formatted before being staged in <code>magit</code>,
                    preventing formatting noise from polluting Git commits.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">flymake</div>
<div class="eco-sub">Diagnostics</div>
</div>
</div>
<p class="eco-desc">
                    Formatting often resolves <code>flymake</code> syntax
                    warnings automatically. The async nature of
                    <code>apheleia</code> ensures the subsequent
                    <code>flymake</code> re-check does not compound UI lag.
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
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">apheleia</div>
<div class="stack-role">Primary Formatter</div>
<div class="stack-desc">
                      Runs external CLI formatters (e.g.,
                      <code>prettier</code>, <code>black</code>,
                      <code>rustfmt</code>) asynchronously in the background,
                      computes a safe diff, and applies it without shifting the
                      cursor.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot</div>
<div class="stack-role">LSP Fallback</div>
<div class="stack-desc">
                      Built-in. Provides <code>eglot-format-buffer</code> which
                      directly invokes <code>textDocument/formatting</code>.
                      Used only when a language lacks a standard CLI formatter.
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
<div class="stack-name">project.el / .dir-locals.el</div>
<div class="stack-role">Project Resolution</div>
<div class="stack-desc">
<code>apheleia</code> automatically discovers the correct
                      formatter based on the major mode and project root
                      configuration.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">before-save-hook</div>
<div class="stack-role">Trigger Mechanism</div>
<div class="stack-desc">
                      Automatically formats the buffer upon saving, ensuring the
                      file is always clean before committing.
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
<td>Format current buffer</td>
<td><code>apheleia-format-buffer</code></td>
<td><kbd>SPC c f</kbd></td>
<td>
                        Async, cursor-preserving whole-file formatting
                        (Recommended).
                      </td>
</tr>
<tr>
<td>LSP Format buffer</td>
<td><code>eglot-format-buffer</code></td>
<td><kbd>C-c C-f</kbd></td>
<td>
                        Synchronous LSP fallback. Blocks UI briefly on large
                        files.
                      </td>
</tr>
<tr>
<td>Toggle format on save</td>
<td><code>apheleia-global-mode</code></td>
<td>—</td>
<td>
                        Enables automatic formatting before every
                        <code>save-buffer</code>.
                      </td>
</tr>
<tr>
<td>Format specific region</td>
<td><code>apheleia-format-buffer</code> (with region)</td>
<td><kbd>SPC c f</kbd> (visual)</td>
<td>
                        Formats only the active region (delegates to CLI
                        <code>--range</code> flags if supported).
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
<span class="fname">init-formatting.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
<pre><code class="language-lisp">;; ==========================================
;; 1. APHELEIA (Async Whole-File Formatting)
;; ==========================================
(use-package apheleia
  :ensure t
  :init
  ;; Enable global auto-formatting on save.
  (apheleia-global-mode +1)
  :custom
  ;; Do not show a message in the echo area after successful formatting.
  (apheleia-hide-log-buffers t)
  ;; Maximum file size (in bytes) to format. Prevents freezing on massive files.
  (apheleia-max-file-size 500000)
  :config
  ;; Optional: Explicitly map modes to formatters if auto-detection fails.
  ;; (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  ;; (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)
  )

;; ==========================================
;; 2. EGLOT LSP FORMATTING (Fallback)
;; ==========================================
;; eglot natively supports `textDocument/formatting` via `eglot-format-buffer`.
;; It is kept available for languages where CLI formatters are unavailable
;; or when LSP-specific formatting rules (e.g., clangd's specific style) are required.
;; No explicit config needed; bound to `C-c C-f` by default in `eglot-mode-map`.

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c f" '(apheleia-format-buffer :wk "Format buffer (async)"))</code></pre>
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
<h4>✓ apheleia · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Thread Blocking</span>
<span class="val"><b>Zero.</b> Runs in a background process; UI remains
                        fully responsive.</span>
</div>
<div class="vs-row">
<span class="lab">Cursor/Undo</span>
<span class="val"><b>Perfect.</b> Computes a diff and applies it,
                        preserving cursor position, mark, and undo
                        history.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Bypasses LSP for formatting, but honors the
                        <code>eglot</code>-only stack mandate for all other
                        features.</span>
</div>
<div class="vs-row">
<span class="lab">Formatter Parity</span>
<span class="val">Uses the <i>exact same</i> CLI tools the LSP server
                        uses under the hood.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ Pure eglot-format-buffer · Fallback</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Thread Blocking</span>
<span class="val"><b>High.</b> Synchronous; blocks the main thread until
                        the LSP server responds.</span>
</div>
<div class="vs-row">
<span class="lab">Cursor/Undo</span>
<span class="val"><b>Poor.</b> Often resets cursor to the top of the
                        buffer and fragments undo history.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors
                        <code>textDocument/formatting</code> natively.</span>
</div>
<div class="vs-row">
<span class="lab">Formatter Parity</span>
<span class="val">Relies entirely on the LSP server's internal formatting
                        logic.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Native treesit Mode Integration</div>
<p class="enh-desc">
<code>apheleia</code> seamlessly recognizes Emacs 31's
                    native <code>*-ts-mode</code> major modes (e.g.,
                    <code>python-ts-mode</code>,
                    <code>typescript-ts-mode</code>) and applies the correct
                    formatter without requiring manual
                    <code>apheleia-mode-alist</code> remapping in most cases.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Improved Process Management</div>
<p class="enh-desc">
                    Emacs 31's refined asynchronous process handling ensures
                    that
                    <code>apheleia</code>'s background formatter processes are
                    cleanly reaped and do not leave zombie processes, even if
                    the buffer is killed mid-format.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
                    Eglot's eglot-format-buffer Refinements
                  </div>
<p class="enh-desc">
                    If you must use the LSP fallback, Emacs 31's
                    <code>eglot</code> has improved error handling for
                    <code>textDocument/formatting</code> failures, gracefully
                    falling back to a no-op rather than throwing opaque JSON-RPC
                    errors into the <code>*Messages*</code> buffer.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

