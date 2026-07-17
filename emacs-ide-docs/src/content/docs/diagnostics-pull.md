---
title: "Diagnostics (Pull Model)"
category: "Diagnostics & Symbols"
status: "Working"
parity: "\"Re-run document diagnostics\" or workspace-wide diagnostic refresh"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Diagnostics (Pull Model)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Symbols</div>
<div class="parity">
<b>VS Code Parity</b>
<span>"Re-run document diagnostics" or workspace-wide diagnostic
            refresh</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/diagnostic</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>workspace/diagnostic</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>flymake-start<span class="route-arrow">→</span>eglot<span class="route-arrow">→</span>LSP pull request<span class="route-arrow">→</span>flymake
              aggregation</code>
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
<td>"Re-run document diagnostics" command</td>
<td>
<kbd>SPC c !</kbd> (<code>flymake-start</code>) triggers
                        an immediate pull request.
                      </td>
</tr>
<tr>
<td>Workspace-wide diagnostic refresh</td>
<td>
<kbd>SPC c P</kbd>
                        (<code>flymake-show-project-diagnostics</code>) pulls
                        and aggregates all workspace errors.
                      </td>
</tr>
<tr>
<td>Centralized Problems panel</td>
<td>
<code>flymake-show-project-diagnostics</code> or
                        <code>consult-flymake-project</code>.
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
<td>On-demand checking (no auto-check)</td>
<td>
                        Disable <code>flymake-mode</code> globally, and bind
                        <code>flymake-start</code> to a convenient key for
                        manual pulls.
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
                    Natively intercepts <code>flymake-start</code> and
                    translates it into an LSP 3.17+
                    <code>textDocument/diagnostic</code>
                    request, respecting the server's
                    <code>interFileDependencies</code> and
                    <code>workspaceDiagnostics</code> capabilities.
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
<code>consult-flymake-project</code> leverages
                    <code>vertico</code> and <code>orderless</code> to provide
                    instant, space-separated fuzzy filtering of the pulled
                    workspace diagnostics, complete with live buffer previews.
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
                    updating instantly when pull diagnostics are refreshed.
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
                    Provides Unimpaired-style <kbd>[e</kbd> / <kbd>]e</kbd>
                    bracket navigation for rapidly cycling through the newly
                    pulled errors without leaving normal state.
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
                      Built-in. Natively implements LSP 3.17+ pull diagnostic
                      capabilities, routing <code>textDocument/diagnostic</code>
                      requests when triggered.
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
                      Built-in. Acts as the central aggregator. When
                      <code>flymake-start</code> is invoked, it signals
                      <code>eglot</code> to pull fresh diagnostics rather than
                      waiting for the server's push interval.
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
<div class="stack-role">Workspace Aggregation</div>
<div class="stack-desc">
                      Emacs 31 native command that lists all pulled diagnostics
                      across the entire workspace in a single, filterable
                      tabulated buffer.
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
<div class="stack-name">consult-flymake-project</div>
<div class="stack-role">Fuzzy Filtering</div>
<div class="stack-desc">
                      Provides a Vertico-powered, live-preview dropdown for
                      rapidly searching and jumping to pulled workspace
                      diagnostics.
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
<td>Force document re-check</td>
<td><code>flymake-start</code></td>
<td><kbd>SPC c !</kbd></td>
<td>
                        Explicitly triggers a pull-model
                        <code>textDocument/diagnostic</code> request for the
                        current buffer.
                      </td>
</tr>
<tr>
<td>Workspace diagnostics list</td>
<td><code>flymake-show-project-diagnostics</code></td>
<td><kbd>SPC c P</kbd></td>
<td>
                        Emacs 31 NEW — pulls and lists all workspace diagnostics
                        in a centralized tabulated buffer.
                      </td>
</tr>
<tr>
<td>Project diagnostics search</td>
<td><code>consult-flymake-project</code></td>
<td><kbd>SPC c E</kbd></td>
<td>
                        Fuzzy-searches all pulled diagnostics across the entire
                        workspace with live preview.
                      </td>
</tr>
<tr>
<td>Toggle auto-check</td>
<td><code>flymake-mode</code></td>
<td>—</td>
<td>
                        Can be toggled off to rely exclusively on manual
                        pull-model requests, saving network/CPU cycles.
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
<span class="fname">init-diagnostics-pull.el</span>
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
  (flymake-indicator-type 'auto)
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
  "c !" '(flymake-start :wk "Force recheck (pull diagnostics)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics panel"))</code></pre>
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
<h4>✓ eglot + flymake native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works seamlessly with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 synergy</span>
<span class="val">Leverages Emacs 31's new
                        <code>flymake-show-project-diagnostics</code> for native
                        workspace pull aggregation.</span>
</div>
<div class="vs-row">
<span class="lab">Flexibility</span>
<span class="val">Allows users to disable <code>flymake-mode</code>
                        entirely and rely only on manual
                        <code>flymake-start</code> pulls for maximum performance
                        on slow servers.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / lsp-ui · Rejected</h4>
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
<span class="lab">Emacs 31 synergy</span>
<span class="val">Relies on legacy, third-party workspace diagnostic
                        buffers.</span>
</div>
<div class="vs-row">
<span class="lab">Flexibility</span>
<span class="val">Tightly couples auto-checking with the LSP client
                        lifecycle.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">LSP 3.17+ Native Support</div>
<p class="enh-desc">
<code>eglot</code> in Emacs 31 fully implements the
                    <code>textDocument/diagnostic</code> and
                    <code>workspace/diagnostic</code> methods. When
                    <code>flymake-start</code> is called, <code>eglot</code>
                    intelligently routes this as a pull request rather than
                    waiting for the server's push interval, providing instant
                    feedback on demand.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">
                    flymake-show-project-diagnostics (NEW)
                  </div>
<p class="enh-desc">
                    A game-changer for workspace pull diagnostics. Instead of
                    querying files individually, this Emacs 31 command requests
                    and lists every diagnostic across the entire workspace in
                    one <code>*Flymake diagnostics*</code> tabulated buffer,
                    perfectly mirroring VS Code's Problems panel.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Dynamic Column Widths</div>
<p class="enh-desc">
                    The tabulated list dynamically adjusts column widths to fit
                    content, preventing truncation of long file paths or verbose
                    LSP pull diagnostic messages in monorepos.
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
<td>Pull Diagnostics Not Triggering</td>
<td>
                        Verify Server Support: Check the
                        <code>*eglot-events*</code> buffer or run
                        <kbd>M-x eglot-describe-connection</kbd> to confirm the
                        server advertises <code>diagnosticProvider</code> in its
                        capabilities. Check Flymake State: Ensure
                        <code>flymake-mode</code> is active in the buffer.
                      </td>
</tr>
<tr>
<td>Workspace Pull is Slow</td>
<td>
                        If <code>workspace/diagnostic</code> requests cause
                        noticeable lag on massive projects, rely on buffer-local
                        pulls (<kbd>SPC c !</kbd>) for immediate feedback, and
                        reserve <code>consult-flymake-project</code> (<kbd>SPC c E</kbd>) for targeted, fuzzy-filtered searches rather than
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

