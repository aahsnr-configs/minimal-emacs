---
title: "Selection Range (Smart Expand/Shrink)"
category: "Formatting & Editing"
status: "Working"
parity: "Shift+Alt+Right / Shift+Alt+Left grows/shrinks selection by syntactic scope"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Selection Range (Smart Expand/Shrink)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Formatting &amp; Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Shift+Alt+Right / Shift+Alt+Left grows/shrinks selection by
            syntactic scope</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/selectionRange</code>
<span aria-hidden="true" class="meta-sep">·</span>
<span style="color: var(--text-dim); font-size: 12px">(Explicitly bypassed)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>treesit<span class="route-arrow">→</span>expreg<span class="route-arrow">→</span>repeat-mode</code>
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
<td><kbd>Shift+Alt+Right</kbd> expands selection</td>
<td>
<kbd>M-=</kbd> or <kbd>v</kbd> (in visual state)
                        triggers <code>expreg-expand</code>.
                      </td>
</tr>
<tr>
<td><kbd>Shift+Alt+Left</kbd> shrinks selection</td>
<td>
<kbd>C-M--</kbd> triggers <code>expreg-contract</code>.
                      </td>
</tr>
<tr>
<td>Rapid tapping to expand further</td>
<td>
<code>repeat-mode</code> allows pressing
                        <kbd>=</kbd> repeatedly after the initial
                        <kbd>M-=</kbd>.
                      </td>
</tr>
<tr>
<td>
                        Expands by semantic units (word → line → function)
                      </td>
<td>
<code>treesit</code> walks the AST to select the next
                        logical syntactic node.
                      </td>
</tr>
<tr>
<td>Canceling selection restores original cursor</td>
<td>
<kbd>C-g</kbd> aborts the loop and
                        <code>expreg-restore-point-on-quit</code> snaps the
                        cursor back.
                      </td>
</tr>
<tr>
<td>Works across all supported languages</td>
<td>
                        Native <code>treesit</code> modes
                        (<code>python-ts-mode</code>, <code>rust-ts-mode</code>,
                        etc.) provide uniform AST structures.
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
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">AST Foundation</div>
</div>
</div>
<p class="eco-desc">
                    Provides the foundational AST that <code>expreg</code>
                    queries, ensuring selection boundaries align perfectly with
                    syntactic scopes (e.g., not selecting mid-string or
                    mid-comment).
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">evil</div>
<div class="eco-sub">Modal Integration</div>
</div>
</div>
<p class="eco-desc">
                    The configuration elegantly hijacks the <kbd>v</kbd> (visual
                    state) key, allowing Vim users to initiate an AST-aware
                    expansion loop without leaving their native modal workflow.
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
<div class="eco-name">general.el</div>
<div class="eco-sub">Keybindings</div>
</div>
</div>
<p class="eco-desc">
                    Eagerly registers the <kbd>M-=</kbd> and
                    <kbd>C-M--</kbd> fallback bindings at startup, ensuring they
                    are available globally without deferred-registration traps.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div>
<div class="eco-name">repeat-mode</div>
<div class="eco-sub">Fluid Loops</div>
</div>
</div>
<p class="eco-desc">
                    Emacs 29+ introduced <code>repeat-mode</code>, which
                    <code>expreg</code> utilizes to create a fluid,
                    modifier-less expansion loop.
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
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">treesit</div>
<div class="stack-role">AST Engine</div>
<div class="stack-desc">
                      Built-in. Provides O(1) C-level syntax tree traversal to
                      identify nested structural boundaries (e.g., word → string
                      → statement → function → class).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">expreg</div>
<div class="stack-role">Expansion Engine</div>
<div class="stack-desc">
                      Authored by Yuan Fu (the implementer of Emacs's built-in
                      tree-sitter support), this package incrementally expands
                      and contracts the active region using the native AST.
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
<div class="stack-name">repeat-mode</div>
<div class="stack-role">Repeat Integration</div>
<div class="stack-desc">
                      Built-in. Allows modifier-less tapping (e.g.,
                      <kbd>M-= = =</kbd>) for rapid, fluid expansion loops
                      without holding down modifier keys.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">evil + general.el</div>
<div class="stack-role">Evil Integration</div>
<div class="stack-desc">
                      Hijacks the <kbd>v</kbd> (visual state) key for seamless,
                      Vim-native AST expansion, while providing global fallbacks
                      (<kbd>M-=</kbd>) for standard Emacs muscle memory.
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
<td>Expand selection</td>
<td><code>expreg-expand</code></td>
<td><kbd>M-=</kbd> / <kbd>v</kbd> (visual)</td>
<td>
                        Grows the active region to the next logical AST node.
                      </td>
</tr>
<tr>
<td>Contract selection</td>
<td><code>expreg-contract</code></td>
<td><kbd>C-M--</kbd></td>
<td>
                        Shrinks the active region to the previous logical AST
                        node.
                      </td>
</tr>
<tr>
<td>Repeat expansion</td>
<td><code>repeat</code></td>
<td><kbd>=</kbd> (after <kbd>M-=</kbd>)</td>
<td>Modifier-less tapping for rapid expansion loops.</td>
</tr>
<tr>
<td>Repeat contraction</td>
<td><code>repeat</code></td>
<td><kbd>-</kbd> (after <kbd>C-M--</kbd>)</td>
<td>
                        Modifier-less tapping for rapid contraction loops.
                      </td>
</tr>
<tr>
<td>Cancel &amp; restore</td>
<td><code>keyboard-quit</code></td>
<td><kbd>C-g</kbd></td>
<td>
                        Aborts the expansion loop and restores the exact
                        original cursor position.
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
<span class="fname">init-selection-range.el</span>
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
;; EXPREG (AST-Aware Expand Region, Reborn)
;; ==========================================
(use-package expreg
  :if (treesit-available-p)
  :defer t
  :commands (expreg-expand expreg-contract)
  :config
  ;; Restores exact cursor origin upon `C-g` (keyboard-quit) to prevent spatial drift.
  (setq expreg-restore-point-on-quit t)
  ;; Emacs 29+ `repeat-mode` integration for modifier-less expansion loops.
  (defvar-keymap expreg-repeat-map
    :doc "Keymap for repeating expreg commands."
    "+" #'expreg-expand
    "=" #'expreg-expand
    "-" #'expreg-contract
    "_" #'expreg-contract)
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map)
  ;; Visual State Routing: Hijacks `v` for seamless AST expansion loops in Evil.
  (general-define-key
    :states 'visual
    "v" #'expreg-expand)
  ;; Global Fallback: Maps `M-=` (Doom's expand-region mnemonic) and modifier combinations.
  (general-define-key
    :states '(normal visual motion)
    "M-=" #'expreg-expand
    "C-M-+" #'expreg-expand
    "C-M--" #'expreg-contract))</code></pre>
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
<h4>✓ expreg + treesit · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>0ms.</b> Executes via native C-level AST pointer
                        arithmetic.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val"><b>Perfect.</b> Works offline and is independent of
                        language server implementation quirks.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate by
                        bypassing unnecessary LSP UI features.</span>
</div>
<div class="vs-row">
<span class="lab">State Preservation</span>
<span class="val"><code>expreg-restore-point-on-quit</code> guarantees
                        that pressing <kbd>C-g</kbd> returns the cursor to its
                        exact starting position.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ LSP textDocument/selectionRange · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>High.</b> Requires a synchronous network roundtrip
                        to the language server on every expansion step.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val"><b>Variable.</b> Depends entirely on the language
                        server's <code>selectionRangeProvider</code> capability,
                        which is often incomplete or buggy.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Adds redundant network overhead for a feature Emacs
                        handles natively.</span>
</div>
<div class="vs-row">
<span class="lab">State Preservation</span>
<span class="val">LSP implementations often lose track of the original
                        anchor point during iterative expansions.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
<code>treesit</code> Native Integration
                  </div>
<p class="enh-desc">
<code>expreg</code> was explicitly designed by the author of
                    Emacs's built-in tree-sitter support to leverage the C-level
                    AST directly, making it exponentially faster and more
                    accurate than legacy regex-based tools like
                    <code>expand-region.el</code>.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title"><code>repeat-mode</code> Synergy</div>
<p class="enh-desc">
                    Emacs 29+ introduced <code>repeat-mode</code>, which
                    <code>expreg</code> utilizes to create a fluid,
                    modifier-less expansion loop. Once you press <kbd>M-=</kbd>,
                    subsequent presses of <kbd>=</kbd> or <kbd>-</kbd> continue
                    the expansion/contraction without requiring you to hold down
                    the Meta key.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
<code>treesit-cycle-sexp-thing</code>
</div>
<p class="enh-desc">
                    While <code>expreg</code> handles block-level AST expansion,
                    Emacs 31's native
                    <code>treesit-cycle-sexp-thing</code> allows you to
                    dynamically toggle between <code>list</code> and
                    <code>sexp</code> navigation paradigms on the fly, providing
                    granular control over how structural boundaries are
                    interpreted during manual selection.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

