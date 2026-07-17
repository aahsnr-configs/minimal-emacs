---
title: "Quick Fix Lightbulb"
category: "Code Actions & Refactoring"
status: "Working"
parity: "Gutter lightbulb icon indicating available code actions or quick fixes"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Quick Fix Lightbulb</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Code Actions &amp; Refactoring</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Gutter lightbulb icon indicating available code actions or quick
            fixes</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/codeAction</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-code-action-indications<span class="route-arrow">→</span>margin/eldoc-hint rendering</code>
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
<td>Lightbulb icon appears in gutter</td>
<td>
<code>eglot-code-action-indications</code> set to
                        <code>margin</code> renders the indicator in the left
                        fringe.
                      </td>
</tr>
<tr>
<td>Indicator disappears when no actions exist</td>
<td>
<code>eglot</code> automatically clears the margin
                        indicator when <code>textDocument/codeAction</code>
                        returns empty.
                      </td>
</tr>
<tr>
<td>Click indicator to open actions</td>
<td>
                        Emacs 31 <code>eglot-code-actions-at-mouse</code> allows
                        invoking the menu by clicking the diagnostic or
                        indicator.
                      </td>
</tr>
<tr>
<td>Fallback hint in status area</td>
<td>
<code>eldoc-hint</code> in
                        <code>eglot-code-action-indications</code> surfaces a
                        subtle hint in the echo area.
                      </td>
</tr>
<tr>
<td>No lag or stutter while typing</td>
<td>
                        Idle delay and debouncing prevent the LSP server from
                        being spammed on every keystroke.
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
                    live buffer previews, making it easy to distinguish between
                    similar refactoring options.
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
                    Often used in tandem; while <code>eglot</code> handles
                    semantic refactoring (e.g., "Extract Method"),
                    <code>apheleia</code> ensures the resulting code is
                    instantly formatted on save without blocking the editor.
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
                    Eagerly registers the <kbd>SPC c a</kbd> leader binding,
                    ensuring the command is instantly available without waiting
                    for package lazy-loading.
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
                      Built-in. Queries <code>textDocument/codeAction</code> on
                      cursor idle and evaluates available actions.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-code-action-indications</div>
<div class="stack-role">Indicator Engine</div>
<div class="stack-desc">
                      Natively renders visual cues in the left margin or via
                      ElDoc hints when actions are present.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M9 18h6M10 22h4M12 2a7 7 0 0 0-4 12.7V17h8v-2.3A7 7 0 0 0 12 2z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-code-action-indicator</div>
<div class="stack-role">Visual Glyph</div>
<div class="stack-desc">
                      Customizable string or glyph (e.g., "💡" or "⚡") used as
                      the visual marker.
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
<div class="stack-name">Idle delay integration</div>
<div class="stack-role">Performance Guard</div>
<div class="stack-desc">
                      Debounces the code action query to prevent main-thread
                      blocking during rapid cursor movement.
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
<td>Toggle margin indicator</td>
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
<span class="fname">init-quick-fix-lightbulb.el</span>
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
 ;; QUICK FIX LIGHTBULB (Built-in Eglot)
 ;; ==========================================
 (use-package eglot
   :ensure nil
   :custom
   ;; Emacs 31 NEW: Control where the lightbulb indicator appears.
   ;; 'margin renders in the left gutter; 'eldoc-hint shows in the echo area.
   (eglot-code-action-indications '(margin eldoc-hint))
   ;; The actual glyph used as the indicator.
   ;; Defaults to a lightbulb emoji, but a simpler Unicode character (e.g., "⚡")
   ;; prevents rendering glitches in specific terminals or tree-sitter modes.
   (eglot-code-action-indicator "💡")
   :config
   ;; Ensure code actions integrate cleanly with consult/vertico for live preview.
   (when (boundp 'eglot-extend-to-xref)
     (setq eglot-extend-to-xref t)))
 ;; ==========================================
 ;; GENERAL.EL KEYBINDINGS (registered eagerly)
 ;; ==========================================
 (ar/global-leader
   "c" '(:ignore t :wk "code")
   "c a" '(eglot-code-actions :wk "Code actions (lightbulb)"))</code></pre>
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
<span class="val">Works exclusively with <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Native margin or ElDoc rendering with zero layout
                        shift.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Zero additional packages; leverages native Emacs
                        completion and margin APIs.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-sideline · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
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
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">
                    eglot-code-action-indications (NEW)
                  </div>
<p class="desc">
                    Emacs 31 introduces native visual indication of available
                    code actions directly within <code>eglot</code>. The
                    variable accepts a list of valid symbols:
                    <code>margin</code> (renders in the left margin),
                    <code>eldoc-hint</code> (renders via ElDoc), or
                    <code>mode-line</code>.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">eglot-code-action-indicator (NEW)</div>
<p class="desc">
                    Customizable string or glyph used as the visual indicator.
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
<p class="desc">
                    Emacs 31's refined <code>eglot</code> pipelines ensure that
                    complex code actions (which require
                    <code>codeAction/resolve</code>
                    network calls) are handled asynchronously, preventing
                    main-thread blocking while the action menu populates.
                  </p>
</div>
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
<td>Indicator Causes Terminal Rendering Glitches</td>
<td>
                        If the default lightbulb emoji causes display corruption
                        in a TTY or specific terminal emulator, change
                        <code>eglot-code-action-indicator</code> to a standard
                        ASCII or simple Unicode character like
                        <code>"*"</code> or <code>"⚡"</code>.
                      </td>
</tr>
<tr>
<td>Indicator Feels Laggy</td>
<td>
                        If the indicator appears slowly, the language server
                        might be slow to respond to
                        <code>textDocument/codeAction</code>. Ensure
                        <code>eglot</code>'s idle delay is not set too low, or
                        consider disabling the <code>margin</code> indication
                        and relying solely on <code>eldoc-hint</code> to reduce
                        rendering overhead.
                      </td>
</tr>
<tr>
<td>Indicator Persists After Actions Are Resolved</td>
<td>
                        Ensure <code>eglot</code> is actively managing the
                        buffer. If the indicator gets stuck, manually trigger
                        <kbd>M-x eglot-code-actions</kbd> to force a state
                        refresh, or restart the server via
                        <kbd>M-x eglot-reconnect</kbd>.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

