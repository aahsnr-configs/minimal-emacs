---
title: "Diagnostics (Push Model)"
category: "Diagnostics & Symbols"
status: "Working"
parity: "Red/green squiggles in the editor and a centralized, filterable \"Problems\" panel"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Diagnostics (Push Model)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Symbols</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Red/green squiggles in the editor and a centralized, filterable
            "Problems" panel</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/publishDiagnostics</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>flymake<span class="route-arrow">→</span>consult-flymake / native *Flymake diagnostics* buffer</code>
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
<td>Squiggles under erroneous code</td>
<td>
<code>flymake</code> renders squiggles via
                        <code>flymake-error</code> /
                        <code>flymake-warning</code> faces.
                      </td>
</tr>
<tr>
<td>Centralized Problems panel</td>
<td>
<code>flymake-show-buffer-diagnostics</code> or
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
                        fringe/margin clicks (Emacs 31).
                      </td>
</tr>
<tr>
<td>Inline message on hover</td>
<td>
                        Emacs 31 <code>'fancy</code> end-of-line rendering
                        displays the message contextually.
                      </td>
</tr>
<tr>
<td>"Quick Fix" from panel</td>
<td>
<kbd>SPC c a</kbd> (<code>eglot-code-actions</code>) at
                        the diagnostic location.
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
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
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
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">evil-collection</div>
<div class="eco-sub">Modal Navigation</div>
</div>
</div>
<p class="eco-desc">
                    Provides Unimpaired-style <kbd>[e</kbd> / <kbd>]e</kbd>
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
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">
                      flymake-show-diagnostics-at-end-of-line
                    </div>
<div class="stack-role">Inline Rendering</div>
<div class="stack-desc">
                      Emacs 31 native feature that lays out diagnostic messages
                      below the affected line using Unicode graphics,
                      eliminating the need for <code>flyover</code>.
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
<td>Buffer diagnostics list</td>
<td><code>flymake-show-buffer-diagnostics</code></td>
<td><kbd>SPC c e</kbd></td>
<td>
                        Opens a tabulated list of errors/warnings for the
                        current file.
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
<td>Next error</td>
<td><code>flymake-goto-next-error</code></td>
<td><kbd>SPC c n</kbd> / <kbd>]e</kbd></td>
<td>
                        Jumps to the next diagnostic in the current buffer.
                      </td>
</tr>
<tr>
<td>Previous error</td>
<td><code>flymake-goto-prev-error</code></td>
<td><kbd>SPC c p</kbd> / <kbd>[e</kbd></td>
<td>
                        Jumps to the previous diagnostic in the current buffer.
                      </td>
</tr>
<tr>
<td>Force recheck</td>
<td><code>flymake-start</code></td>
<td><kbd>SPC c !</kbd></td>
<td>
                        Manually triggers a diagnostic refresh (useful for
                        pull-model fallbacks).
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
<span class="fname">init-diagnostics-push.el</span>
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
;; 1. FLYMAKE CORE (Emacs 31 Native Diagnostics)
;; ==========================================
(use-package flymake
  :ensure nil
  :custom
  ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line,
  ;; effectively replacing the `flyover` package with richer native output.
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  ;; Emacs 31 NEW: 'auto prefers fringes on GUI frames, falls back to margins on TTY.
  (flymake-indicator-type 'auto)
  ;; Suppress the legacy echo-area summary to keep the minibuffer clean for eldoc.
  (flymake-suppress-zero-count-warnings t)
  :config
  ;; Enable flymake globally in programming buffers.
  (add-hook 'prog-mode-hook #'flymake-mode))

;; ==========================================
;; 2. CONSULT-FLYMAKE (Vertico-powered filtering)
;; ==========================================
(use-package consult-flymake
  :ensure nil  ;; Bundled with consult
  :after (consult flymake)
  :config
  ;; `consult-flymake-project` is not a native command; it requires a wrapper
  ;; to pass the prefix argument for project-wide searching.
  (defun consult-flymake-project ()
    "Invoke consult-flymake across all project buffers."
    (interactive)
    (consult-flymake t)))

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c e" '(consult-flymake :wk "Search errors (buffer)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c n" '(flymake-goto-next-error :wk "Next error")
  "c p" '(flymake-goto-prev-error :wk "Prev error")
  "c !" '(flymake-start :wk "Force recheck"))

(general-define-key
  :states 'motion
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)</code></pre>{% endraw %}
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
<span class="val">Leverages new <code>'fancy</code> end-of-line rendering
                        and enhanced tabulated lists.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Built into Emacs core; zero additional packages or
                        background processes.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ flycheck / lsp-ui · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val"><code>flycheck</code> requires explicit checker
                        definitions; <code>lsp-ui</code> is hard-bound to
                        <code>lsp-mode</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Adds redundant diagnostic engine overhead or requires
                        forbidden <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 synergy</span>
<span class="val">Lacks integration with Emacs 31 core UI enhancements or
                        relies on heavy child-frame pipelines.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Spawns independent checker processes or incurs
                        child-frame overhead on every update.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
<code>flymake-show-diagnostics-at-end-of-line 'fancy</code>
</div>
<p class="enh-desc">
                    A game-changer for inline diagnostics. Instead of truncating
                    messages in the echo area or requiring a floating
                    child-frame (<code>flyover</code>), Emacs 31 lays out
                    diagnostics below the affected line using Unicode graphics
                    that point back to the exact locus of the error.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">
<code>flymake-show-buffer-diagnostics</code> Enhanced
                  </div>
<p class="enh-desc">
                    The native diagnostics buffer now highlights the diagnostic
                    nearest to the current point in the listing and responds to
                    fringe/margin mouse clicks, making buffer-local navigation
                    instantaneous. Column widths also dynamically adjust to
                    content.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
<code>flymake-indicator-type 'auto</code>
</div>
<p class="enh-desc">
                    Intelligently prefers fringes on GUI frames for a cleaner
                    look, but gracefully falls back to margins on TTY frames,
                    maintaining visual consistency across all environments.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">
<code>flymake-make-diagnostic</code> API Extensions
                  </div>
<p class="enh-desc">
                    Accepts new <code>origin</code> and
                    <code>code</code> attributes, and
                    <code>flymake-diagnostic-format-alist</code>
                    provides granular, per-context control over how diagnostics
                    are formatted.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

