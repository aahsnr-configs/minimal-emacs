---
title: "Problems Panel"
category: "Diagnostics & Symbols"
status: "Working"
parity: "\"Problems\" tab aggregating workspace diagnostics with severity filtering"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Problems Panel</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Symbols</div>
<div class="parity">
<b>VS Code Parity</b>
<span>"Problems" tab aggregating workspace diagnostics with severity
            filtering</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/publishDiagnostics</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>flymake<span class="route-arrow">→</span>consult-flymake / flymake-show-project-diagnostics</code>
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
<td>Centralized Problems panel</td>
<td>
<kbd>SPC c P</kbd>
                        (<code>flymake-show-project-diagnostics</code>) or
                        <kbd>SPC c E</kbd>
                        (<code>consult-flymake-project</code>).
                      </td>
</tr>
<tr>
<td>Filter by severity (Errors / Warnings)</td>
<td>
                        Native <kbd>/</kbd> filter in tabulated-list +
                        <code>consult-flymake</code> for fuzzy matching.
                      </td>
</tr>
<tr>
<td>Click error to jump to location</td>
<td>
<kbd>RET</kbd> on any row in the diagnostics buffer, or
                        fringe/margin clicks.
                      </td>
</tr>
<tr>
<td>"Quick Fix" from panel</td>
<td>
<kbd>SPC c a</kbd> (<code>eglot-code-actions</code>) at
                        the diagnostic location.
                      </td>
</tr>
<tr>
<td>Squiggles under erroneous code</td>
<td>
<code>flymake</code> renders squiggles via
                        <code>flymake-error</code> /
                        <code>flymake-warning</code>
                        faces.
                      </td>
</tr>
<tr>
<td>Inline message on hover</td>
<td>
                        Emacs 31 <code>'fancy</code> end-of-line rendering
                        displays the message contextually.
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
                    Natively intercepts
                    <code>textDocument/publishDiagnostics</code> and routes the
                    payload directly into <code>flymake</code>'s API, requiring
                    zero custom translation layers.
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
<div class="eco-name">consult</div>
<div class="eco-sub">Fuzzy Filtering</div>
</div>
</div>
<p class="eco-desc">
<code>consult-flymake</code> leverages
                    <code>vertico</code> and <code>orderless</code> to provide
                    instant, space-separated fuzzy filtering of diagnostic
                    messages, complete with live buffer previews.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">doom-modeline</div>
<div class="eco-sub">Status Bar Integration</div>
</div>
</div>
<p class="eco-desc">
                    The <code>doom-modeline-lsp</code> segment automatically
                    displays live error and warning counts in the mode line,
                    mirroring the VS Code status bar badge.
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
<div class="eco-name">evil-collection</div>
<div class="eco-sub">Modal Navigation</div>
</div>
</div>
<p class="eco-desc">
                    Provides Unimpaired-style <kbd>[ e</kbd> / <kbd>] e</kbd>
                    bracket navigation for rapidly cycling through errors
                    without leaving normal state.
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
                      Built-in. Receives
                      <code>textDocument/publishDiagnostics</code> payloads and
                      translates them into native <code>flymake</code>
                      diagnostics.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">flymake</div>
<div class="stack-role">Diagnostic Engine</div>
<div class="stack-desc">
                      Built-in. Manages the lifecycle of diagnostics, rendering
                      squiggles and aggregating them into tabulated lists.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">
                      flymake-show-project-diagnostics
                    </div>
<div class="stack-role">Workspace Aggregator</div>
<div class="stack-desc">
                      Emacs 31 NEW. Natively lists all pulled diagnostics across
                      the entire workspace in a single, filterable tabulated
                      buffer.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">consult-flymake</div>
<div class="stack-role">Fuzzy Filtering</div>
<div class="stack-desc">
                      Provides a Vertico-powered, live-preview dropdown for
                      rapidly searching and jumping to diagnostics across the
                      buffer or project.
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
<td>Buffer diagnostics search</td>
<td><code>consult-flymake</code></td>
<td><kbd>SPC c e</kbd></td>
<td>
                        Fuzzy-searches diagnostics in the current buffer with
                        live preview.
                      </td>
</tr>
<tr>
<td>Project diagnostics search</td>
<td><code>consult-flymake-project</code></td>
<td><kbd>SPC c E</kbd></td>
<td>
                        Fuzzy-searches all diagnostics across the entire
                        workspace.
                      </td>
</tr>
<tr>
<td>Project diagnostics panel</td>
<td><code>flymake-show-project-diagnostics</code></td>
<td><kbd>SPC c P</kbd></td>
<td>
                        Emacs 31 NEW — opens a centralized, filterable tabulated
                        list of all project errors.
                      </td>
</tr>
<tr>
<td>Next error</td>
<td><code>flymake-goto-next-error</code></td>
<td><kbd>SPC c n</kbd> / <kbd>] e</kbd></td>
<td>
                        Jumps to the next diagnostic in the current buffer.
                      </td>
</tr>
<tr>
<td>Previous error</td>
<td><code>flymake-goto-prev-error</code></td>
<td><kbd>SPC c p</kbd> / <kbd>[ e</kbd></td>
<td>
                        Jumps to the previous diagnostic in the current buffer.
                      </td>
</tr>
<tr>
<td>Force recheck</td>
<td><code>flymake-start</code></td>
<td><kbd>SPC c !</kbd></td>
<td>Manually triggers a diagnostic refresh.</td>
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
<span class="fname">init-problems-panel.el</span>
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
 ;; FLYMAKE CORE (Emacs 31 Native Diagnostics)
 ;; ==========================================
 (use-package flymake
   :ensure nil
   :custom
   ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line.
   (flymake-show-diagnostics-at-end-of-line 'fancy)
   ;; Emacs 31 NEW: 'auto prefers fringes on GUI frames, falls back to margins on TTY.
   (flymake-indicator-type 'auto)
   ;; Suppress the legacy echo-area summary to keep the minibuffer clean.
   (flymake-suppress-zero-count-warnings t)
   :config
   ;; Enable flymake globally in programming buffers.
   (add-hook 'prog-mode-hook #'flymake-mode))
 ;; ==========================================
 ;; CONSULT-FLYMAKE (Vertico-powered filtering)
 ;; ==========================================
 (use-package consult-flymake
   :ensure nil  ;; Bundled with consult
   :after (consult flymake)
   :config
   ;; Wrapper for project-wide diagnostic searching.
   (defun consult-flymake-project ()
     "Invoke consult-flymake across all project buffers."
     (interactive)
     (consult-flymake t)))
 ;; ==========================================
 ;; GENERAL.EL KEYBINDINGS (registered eagerly)
 ;; ==========================================
 (ar/global-leader
   "c" '(:ignore t :wk "code")
   "c e" '(consult-flymake :wk "Search errors (buffer)")
   "c E" '(consult-flymake-project :wk "Search errors (project)")
   "c n" '(flymake-goto-next-error :wk "Next error")
   "c p" '(flymake-goto-prev-error :wk "Prev error")
   "c !" '(flymake-start :wk "Force recheck")
   "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics panel"))
 (general-define-key
   :states 'motion
   "] e" #'flymake-goto-next-error
   "[ e" #'flymake-goto-prev-error)</code></pre>
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
<h4>✓ flymake native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works seamlessly with <code>eglot</code> out of the
                        box.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 synergy</span>
<span class="val">Leverages new
                        <code>flymake-show-project-diagnostics</code> for native
                        workspace aggregation.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Built into Emacs core; zero additional packages or
                        background processes.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui / lsp-mode · Rejected</h4>
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
<span class="lab">Emacs 31 synergy</span>
<span class="val">Relies on legacy, third-party workspace diagnostic
                        buffers.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Heavy child-frame overhead on every diagnostic
                        update.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">
                    flymake-show-project-diagnostics (NEW)
                  </div>
<p class="desc">
                    A game-changer for workspace diagnostics. Instead of
                    querying files individually, this Emacs 31 command requests
                    and lists every diagnostic across the entire workspace in
                    one
                    <code>*Flymake diagnostics*</code> tabulated buffer,
                    perfectly mirroring VS Code's Problems panel.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">
                    flymake-show-diagnostics-at-end-of-line 'fancy
                  </div>
<p class="desc">
                    Instead of truncating messages in the echo area, Emacs 31
                    lays out diagnostics below the affected line using Unicode
                    graphics that point back to the exact locus of the error.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Dynamic Column Widths</div>
<p class="desc">
                    The tabulated list dynamically adjusts column widths to fit
                    content, preventing truncation of long file paths or verbose
                    LSP diagnostic messages in monorepos.
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
<td>Project Diagnostics Panel is Empty</td>
<td>
                        Verify Project Context:
                        <code>flymake-show-project-diagnostics</code> requires
                        an active <code>project-current</code>. Ensure your
                        project has a <code>.git</code> directory or a
                        <code>pyproject.toml</code>/<code>package.json</code> at
                        the root. Check Eglot Connection: Ensure
                        <code>eglot</code> is actively connected.
                      </td>
</tr>
<tr>
<td>Diagnostics Feel Laggy</td>
<td>
                        If workspace diagnostic requests cause noticeable lag on
                        massive projects, rely on buffer-local pulls (<kbd>SPC c !</kbd>) for immediate feedback, and reserve
                        <code>consult-flymake-project</code> (<kbd>SPC c E</kbd>) for targeted, fuzzy-filtered searches rather than
                        full workspace refreshes.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

