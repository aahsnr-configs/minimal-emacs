---
title: "Code Actions (Quick Fixes & Refactorings)"
category: "Code Actions & Refactoring"
status: "Working"
parity: "Ctrl+. lightbulb menu, inline quick fixes, and refactoring options"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Code Actions (Quick Fixes &amp; Refactorings)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Code Actions &amp; Refactoring</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Ctrl+. lightbulb menu, inline quick fixes, and refactoring
            options</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/codeAction</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>codeAction/resolve</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-code-actions<span class="route-arrow">→</span>completing-read (Vertico/Consult)</code>
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
<td><kbd>Ctrl+.</kbd> opens quick fix menu</td>
<td>
<kbd>SPC c a</kbd> (<code>eglot-code-actions</code>)
                        opens Vertico-powered action menu.
                      </td>
</tr>
<tr>
<td>Lightbulb icon in gutter</td>
<td>
                        Emacs 31 <code>eglot-code-action-indications</code>
                        renders 💡 in the left margin.
                      </td>
</tr>
<tr>
<td>"Organize Imports" shortcut</td>
<td>
<kbd>SPC c i</kbd>
                        (<code>eglot-code-action-organize-imports</code>).
                      </td>
</tr>
<tr>
<td>Action menu filters by type (Quick Fix, Refactor)</td>
<td>
<code>vertico</code> allows instant fuzzy filtering
                        (e.g., typing <code>fix</code> or
                        <code>refactor</code>).
                      </td>
</tr>
<tr>
<td>Live preview of the proposed change</td>
<td>
<code>consult</code> integration provides live buffer
                        preview of the action's effect.
                      </td>
</tr>
<tr>
<td>Click lightbulb to open actions</td>
<td>
                        Emacs 31 <code>eglot-code-actions-at-mouse</code> (click
                        diagnostic with <kbd>mouse-2</kbd>).
                      </td>
</tr>
<tr>
<td>Workspace-level commands</td>
<td>
<kbd>SPC c x</kbd>
                        (<code>eglot-execute-command</code>) surfaces
                        server-defined palette entries.
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
                    Natively queries <code>textDocument/codeAction</code> and
                    handles the <code>codeAction/resolve</code> lifecycle,
                    applying the resulting <code>WorkspaceEdit</code> safely.
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
                    Intercepts the <code>completing-read</code> prompt to
                    provide a vertically scrolling, fuzzy-filterable list with
                    live buffer previews.
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
<div class="eco-sub">Post-Refactor Formatting</div>
</div>
</div>
<p class="eco-desc">
                    Often used in tandem; while <code>eglot</code> handles
                    semantic refactoring, <code>apheleia</code> ensures the
                    resulting code is instantly formatted on save.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
</div>
<div>
<div class="eco-name">general.el</div>
<div class="eco-sub">Keybindings</div>
</div>
</div>
<p class="eco-desc">
                    Eagerly registers the <kbd>SPC c a</kbd> and
                    <kbd>SPC c i</kbd> leader bindings, ensuring the commands
                    are instantly available without waiting for lazy-loading.
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
                      Built-in. Queries <code>textDocument/codeAction</code> at
                      point and resolves complex actions via
                      <code>codeAction/resolve</code>.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M9 18h6M10 22h4M12 2a7 7 0 0 0-4 12.7V17h8v-2.3A7 7 0 0 0 12 2z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-code-actions</div>
<div class="stack-role">Action Router</div>
<div class="stack-desc">
                      Presents available actions (quick fixes, refactors, source
                      actions) in a searchable <code>completing-read</code>
                      menu.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">vertico / consult</div>
<div class="stack-role">UI Engine</div>
<div class="stack-desc">
                      Provides fuzzy filtering, candidate grouping, and live
                      preview of the code action context.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-code-action-indications</div>
<div class="stack-role">Visual Indicator</div>
<div class="stack-desc">
                      Emacs 31 NEW. Renders the "lightbulb" indicator in the
                      left margin or inline near the cursor when actions are
                      available.
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
<td>Invoke code actions</td>
<td><code>eglot-code-actions</code></td>
<td><kbd>SPC c a</kbd> / <kbd>C-.</kbd></td>
<td>
                        Opens the searchable menu of all available actions at
                        point.
                      </td>
</tr>
<tr>
<td>Organize imports</td>
<td><code>eglot-code-action-organize-imports</code></td>
<td><kbd>SPC c i</kbd></td>
<td>
                        Direct shortcut for the most common source action.
                      </td>
</tr>
<tr>
<td>Quick fix (diagnostic)</td>
<td><code>eglot-code-action-quickfix</code></td>
<td>—</td>
<td>
                        Filters the code action menu to show only quick fixes
                        for the current diagnostic.
                      </td>
</tr>
<tr>
<td>Execute server command</td>
<td><code>eglot-execute-command</code></td>
<td><kbd>SPC c x</kbd></td>
<td>
                        Runs workspace-level commands surfaced by the server
                        (e.g., "Restart Server").
                      </td>
</tr>
<tr>
<td>Toggle lightbulb indicator</td>
<td><code>eglot-code-action-indications</code></td>
<td>—</td>
<td>
                        Configured via <code>setq</code> to show in
                        <code>margin</code>, <code>eldoc-hint</code>, or both.
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
<span class="fname">init-code-actions.el</span>
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
;; EGLOT CODE ACTIONS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Control where the lightbulb indicator appears.
  ;; - 'margin: Renders indicator in the left margin (VS Code parity)
  ;; - 'eldoc-hint: Renders indicator inline near the point via ElDoc
  ;; Both can be enabled simultaneously for maximum visibility.
  (eglot-code-action-indications '(margin eldoc-hint))
  ;; The actual glyph/string used as the indicator.
  ;; Defaults to a lightbulb emoji, but can be changed to a simpler Unicode
  ;; character (e.g., "⚡" or "✦") if the emoji causes rendering glitches
  ;; in specific tree-sitter modes or terminal emulators.
  (eglot-code-action-indicator "💡")
  :config
  ;; Ensure eglot's code actions integrate cleanly with consult/vertico
  ;; for live preview and fuzzy filtering.
  (when (boundp 'eglot-extend-to-xref)
    (setq eglot-extend-to-xref t)))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c a" '(eglot-code-actions :wk "Code actions (lightbulb)")
  "c i" '(eglot-code-action-organize-imports :wk "Organize imports")
  "c x" '(eglot-execute-command :wk "Execute server command"))</code></pre>
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
<span class="lab">Coupling</span>
<span class="val">Works exclusively with <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Native <code>completing-read</code> menu + Emacs 31
                        margin indicators.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Zero additional packages; leverages native Emacs
                        completion.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-sideline · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Heavy sideline rendering engine that shifts text and
                        causes redisplay jitter.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Child-frame overhead and complex sideline
                        management.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
                    eglot-code-action-indications (NEW)
                  </div>
<p class="enh-desc">
                    Emacs 31 introduces native visual indication of available
                    code actions directly within <code>eglot</code>. The
                    variable accepts a list of symbols:
                    <code>margin</code> (renders in the left margin),
                    <code>eldoc-hint</code> (renders via ElDoc), or
                    <code>mode-line</code>. This completely eliminates the need
                    for third-party sideline packages.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">eglot-code-action-indicator (NEW)</div>
<p class="enh-desc">
                    Customizable string/glyph used as the visual indicator.
                    While the default is a lightbulb emoji, it can be safely
                    swapped for a simpler Unicode character (like
                    <code>⚡</code>) to prevent rendering glitches in specific
                    tree-sitter modes or TTY environments.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
                    Enhanced completing-read Integration
                  </div>
<p class="enh-desc">
                    Emacs 31's refined <code>xref</code> and
                    <code>eglot</code> pipelines ensure that complex code
                    actions (which require <code>codeAction/resolve</code>
                    network calls) are handled asynchronously, preventing
                    main-thread blocking while the action menu populates.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Mouse Integration</div>
<p class="enh-desc">
<code>eglot-code-actions-at-mouse</code> allows invoking the
                    code action menu directly by clicking on a diagnostic
                    squiggle with <kbd>mouse-2</kbd>, matching VS Code's
                    click-to-fix paradigm.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

