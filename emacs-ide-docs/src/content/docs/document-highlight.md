---
title: "Document Highlight"
category: "Navigation & Visual Enhancements"
status: "Working"
parity: "Auto-highlighting of all references to the symbol at the cursor position"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Document Highlight</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Auto-highlighting of all references to the symbol at the cursor
            position</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/documentHighlight</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>:documentHighlightProvider<span class="route-arrow">→</span>native overlay application</code>
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
<td>Auto-highlights symbol on cursor stop</td>
<td>
<code>eglot</code> natively triggers
                        <code>textDocument/documentHighlight</code> on cursor
                        idle.
                      </td>
</tr>
<tr>
<td>Highlights read/write occurrences differently</td>
<td>
<code>eglot</code> parses the <code>kind</code>
                        (read/write/text) from the LSP response and applies
                        distinct faces.
                      </td>
</tr>
<tr>
<td>Highlight clears on cursor move</td>
<td>
                        Overlays are automatically destroyed when the cursor
                        moves to a new symbol or buffer.
                      </td>
</tr>
<tr>
<td>Works across the entire visible buffer</td>
<td>
<code>eglot</code> requests highlights for the current
                        file scope and renders them as buffer overlays.
                      </td>
</tr>
<tr>
<td>Fallback when LSP is slow/disconnected</td>
<td>
                        Native
                        <code>isearch-forward-symbol-at-point</code> (<kbd>M-s .</kbd>) provides instant local regex-based highlighting.
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
                    Natively routes
                    <code>textDocument/documentHighlight</code> payloads to the
                    overlay engine without requiring manual hook registration.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">Fallback Parser</div>
</div>
</div>
<p class="eco-desc">
                    Provides the underlying structural awareness for local
                    fallback highlighting, ensuring that even without LSP,
                    symbol boundaries are respected.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div>
<div class="eco-name">consult</div>
<div class="eco-sub">Navigation Bridge</div>
</div>
</div>
<p class="eco-desc">
                    If the user needs to navigate the highlighted occurrences,
                    <kbd>M-?</kbd> (<code>xref-find-references</code>) instantly
                    bridges the visual highlight to a searchable dropdown.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">doom-themes</div>
<div class="eco-sub">Visual Styling</div>
</div>
</div>
<p class="eco-desc">
                    Ensures the highlight is visible but recessive enough not to
                    compete with <code>hl-line-mode</code> or syntax
                    highlighting.
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
                      Built-in. Queries
                      <code>textDocument/documentHighlight</code> on cursor idle
                      and parses the returned range array.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">Overlay Engine</div>
<div class="stack-role">Rendering</div>
<div class="stack-desc">
                      Natively applies overlays to matching symbols via its
                      <code>:documentHighlightProvider</code> capability.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">highlight face</div>
<div class="stack-role">Visual Styling</div>
<div class="stack-desc">
                      The standard Emacs face applied to the highlighted ranges,
                      customizable via theme.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot--managed-mode</div>
<div class="stack-role">Performance Guard</div>
<div class="stack-desc">
                      Automatically enables highlighting in LSP-managed buffers,
                      respecting idle delays to prevent spam.
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
<td>Cycle through highlights</td>
<td><code>xref-find-references</code></td>
<td><kbd>M-?</kbd></td>
<td>
                        If more context is needed, jump to the full reference
                        list.
                      </td>
</tr>
<tr>
<td>Jump to next occurrence</td>
<td><code>isearch-forward-symbol-at-point</code></td>
<td><kbd>M-s .</kbd></td>
<td>
                        Native Emacs fallback to cycle through local occurrences
                        if LSP is disconnected.
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
<span class="fname">init-highlight.el</span>
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
;; EGLOT DOCUMENT HIGHLIGHT (Built-in)
;; ==========================================
;; Eglot natively handles `textDocument/documentHighlight` via its
;; `:documentHighlightProvider` capability. When the server supports it,
;; Eglot automatically applies overlays to matching symbols on cursor idle.
;; No explicit minor mode hooks or third-party packages are required.

;; ==========================================
;; PERFORMANCE TUNING (Global)
;; ==========================================
;; The global `jit-lock-defer-time` of 0.05 (50ms) perfectly guards
;; eglot's document highlight queries. This prevents the LSP server
;; from being spammed during rapid cursor movement.
(setq jit-lock-defer-time 0.05)

;; ==========================================
;; VISUAL STYLING (Tokyo Night Synergy)
;; ==========================================
(custom-set-faces
 '(highlight ((t (:background "#292e42" :foreground "#c0caf5" :weight bold)))))</code></pre>
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
<span class="lab">Performance</span>
<span class="val">Lightweight overlays, respects
                        <code>jit-lock-defer-time</code> (50ms).</span>
</div>
<div class="vs-row">
<span class="lab">Dependencies</span>
<span class="val">Zero. Built directly into <code>eglot.el</code>.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui / third-party highlighters · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Heavy sideline rendering, prone to main-thread
                        blocking.</span>
</div>
<div class="vs-row">
<span class="lab">Dependencies</span>
<span class="val">Requires <code>lsp-ui</code> and its complex
                        child-frame pipeline.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">jit-lock-defer-time Synergy</div>
<p class="enh-desc">
                    The global <code>jit-lock-defer-time</code> of
                    <code>0.05</code> (50ms) perfectly guards
                    <code>eglot</code>'s document highlight queries. Prevents
                    LSP spam during rapid cursor movement, eliminating
                    micro-stutters.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Native Overlay Efficiency</div>
<p class="enh-desc">
                    Emacs 31's C-level overlay rendering is highly optimized.
                    <code>eglot</code> draws highlight rectangles with zero
                    redisplay lag, even in files with hundreds of occurrences.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Treesit Fallback Readiness</div>
<p class="enh-desc">
                    If the LSP server stalls, <kbd>M-s .</kbd>
                    (<code>isearch-forward-symbol-at-point</code>) utilizes
                    <code>treesit</code> or syntax tables to highlight local
                    occurrences instantly without network latency.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

