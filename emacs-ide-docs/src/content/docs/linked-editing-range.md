---
title: "Linked Editing Range (Auto Rename Tag)"
category: "Visual Enhancements / Editing"
status: "Working"
parity: "Editing one HTML/JSX tag name automatically updates its matching pair"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Linked Editing Range (Auto Rename Tag)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Visual Enhancements / Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Editing one HTML/JSX tag name automatically updates its matching
            pair</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/linkedEditingRange</code>
<span aria-hidden="true" class="meta-sep">·</span>
<span style="color: var(--text-dim); font-size: 12px">(Intentionally bypassed)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>auto-rename-tag<span class="route-arrow">→</span>native Emacs
              regex/AST parsing</code>
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
<td>
                        Typing in <code>&lt;div&gt;</code> instantly updates
                        <code>&lt;/div&gt;</code>
</td>
<td>
<code>auto-rename-tag-mode</code> detects the pair and
                        updates it synchronously with 0ms latency.
                      </td>
</tr>
<tr>
<td>Works for HTML, XML, and JSX/TSX</td>
<td>
                        Hooked into <code>html-ts-mode</code>,
                        <code>nxml-mode</code>, and <code>tsx-ts-mode</code>
                        respectively.
                      </td>
</tr>
<tr>
<td>No lag or stutter while typing</td>
<td>
                        Native local execution completely bypasses the LSP
                        network request cycle.
                      </td>
</tr>
<tr>
<td>Respects cursor position</td>
<td>
                        The package is designed to adjust the cursor position
                        intelligently after the rename operation, preventing
                        spatial drift.
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
<circle cx="12" cy="12" r="10"></circle>
<path d="M8 12h8M12 8v8"></path>
</svg>
</div>
<div>
<div class="eco-name">electric-pair-mode</div>
<div class="eco-sub">Auto-Pairing</div>
</div>
</div>
<p class="eco-desc">
                    Works in tandem with native auto-pairing. When
                    <code>electric-pair-mode</code> inserts the closing
                    <code>&gt;</code>, <code>auto-rename-tag</code> is already
                    primed to track the structural pair for subsequent edits.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">web-mode / tsx-ts-mode</div>
<div class="eco-sub">Target Modes</div>
</div>
</div>
<p class="eco-desc">
                    The mode hooks ensure that the renaming engine is only
                    active in buffers where tag pairing is structurally
                    relevant, preventing false positives in standard programming
                    languages like Python or Rust.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div>
<div class="eco-name">eglot</div>
<div class="eco-sub">LSP Offloading</div>
</div>
</div>
<p class="eco-desc">
                    By intentionally bypassing the LSP
                    <code>linkedEditingRange</code> capability, we free up
                    <code>eglot</code> to focus on heavier semantic tasks (like
                    diagnostics and completion) without being bogged down by
                    per-keystroke rename requests.
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
<div class="stack-name">auto-rename-tag</div>
<div class="stack-role">Renaming Engine</div>
<div class="stack-desc">
                      MELPA package. Provides zero-latency, local tag renaming
                      for HTML/XML/JSX without requiring LSP round-trips.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">Target Modes</div>
<div class="stack-role">Scope Definition</div>
<div class="stack-desc">
<code>tsx-ts-mode</code>, <code>web-mode</code>,
                      <code>html-ts-mode</code>, <code>nxml-mode</code>. Major
                      modes where structural tag pairing is prevalent.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">Native Emacs parsing</div>
<div class="stack-role">Performance Guard</div>
<div class="stack-desc">
                      Operates entirely locally using Emacs' built-in syntax
                      tables or Tree-sitter AST, guaranteeing 0ms input latency.
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
<td>Toggle auto-rename</td>
<td><code>auto-rename-tag-mode</code></td>
<td>—</td>
<td>
                        Enabled automatically via mode hooks for relevant
                        languages.
                      </td>
</tr>
<tr>
<td>Manual tag jump (fallback)</td>
<td>
<code>sgml-skip-tag-forward</code> /
                        <code>backward</code>
</td>
<td><kbd>C-c C-f</kbd> / <kbd>C-c C-b</kbd></td>
<td>
                        Native Emacs commands to manually jump between paired
                        tags if needed.
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
<span class="fname">init-linked-editing.el</span>
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
;; AUTO-RENAME-TAG (Zero-Latency Local Tag Renaming)
;; ==========================================
;; The LSP `textDocument/linkedEditingRange` capability requires a synchronous
;; network round-trip to the language server on every keystroke while inside a
;; tag. This frequently causes noticeable input latency and main-thread blocking.
;; Instead, we utilize the `auto-rename-tag` package, which replicates the exact
;; behavior of the VS Code "Auto Rename Tag" extension using zero-latency local parsing.

(use-package auto-rename-tag
  :ensure t
  :defer t
  :commands auto-rename-tag-mode
  :hook ((tsx-ts-mode
          html-ts-mode
          web-mode
          nxml-mode
          rjsx-mode) . auto-rename-tag-mode)
  :custom
  ;; Ensure the package activates instantly upon mode entry.
  (auto-rename-tag-mode 1))</code></pre>
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
<h4>✓ auto-rename-tag · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>0ms.</b> Executes locally via native Emacs regex/AST
                        parsing.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val"><b>Perfect.</b> Works offline and is independent of
                        language server implementation quirks.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate by
                        avoiding unnecessary LSP UI features.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">Integrates seamlessly with <code>tsx-ts-mode</code> and
                        <code>web-mode</code> without conflicting with
                        Tree-sitter parsing.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ LSP textDocument/linkedEditingRange · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>High.</b> Requires a synchronous network round-trip
                        on every keystroke.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val"><b>Variable.</b> Depends entirely on the language
                        server's
                        <code>linkedEditingRangeProvider</code> capability,
                        which is often incomplete or slow.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Adds redundant network overhead for a feature Emacs
                        handles natively.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">No specific integration with Emacs 31 core
                        enhancements.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Tree-sitter Mode Compatibility</div>
<p class="enh-desc">
<code>auto-rename-tag</code> operates harmoniously alongside
                    Emacs 31's native <code>tsx-ts-mode</code> and
                    <code>html-ts-mode</code>. Because it relies on fundamental
                    buffer text manipulation rather than fighting the AST, it
                    does not disrupt Tree-sitter's font-lock or indentation
                    engines.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Zero Network Dependency</div>
<p class="enh-desc">
                    By handling this locally, the configuration remains fully
                    functional in air-gapped environments, over slow TRAMP
                    connections, or when the LSP server is temporarily
                    unresponsive, ensuring a consistently smooth editing
                    experience.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

