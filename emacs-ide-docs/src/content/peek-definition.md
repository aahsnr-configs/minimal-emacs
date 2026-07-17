---
title: "Peek Definition / Peek References"
category: "Navigation & Code Jumping"
status: "Working"
parity: "Inline expandable panel showing definition or references without tab switching (Alt+F12 / Shift+Alt+F12)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Peek Definition / Peek References</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Inline expandable panel showing definition or references without
            tab switching (Alt+F12 / Shift+Alt+F12)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/definition</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>textDocument/references</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>xref<span class="route-arrow">→</span>peek (overlay rendering)</code>
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
<td><kbd>Alt+F12</kbd> peeks definition inline</td>
<td>
<kbd>SPC c p d</kbd> (<code>peek-xref-definition</code>)
                        renders the target below the cursor.
                      </td>
</tr>
<tr>
<td><kbd>Shift+Alt+F12</kbd> peeks references</td>
<td>
<kbd>SPC c p r</kbd> (<code>peek-xref-references</code>)
                        renders the first reference inline.
                      </td>
</tr>
<tr>
<td>Expandable panel without tab switch</td>
<td>
<code>peek</code> uses buffer overlays, keeping the
                        original buffer fully visible and active.
                      </td>
</tr>
<tr>
<td>Scroll within the peek panel</td>
<td>
                        Native Emacs scrolling (<kbd>C-n</kbd> / <kbd>C-p</kbd>
                        or arrow keys) works within the overlay context.
                      </td>
</tr>
<tr>
<td>Close peek panel</td>
<td>
<kbd>SPC c p h</kbd> (<code>peek-overlay-dwim</code>)
                        hides the overlay instantly.
                      </td>
</tr>
<tr>
<td>Live update on source change</td>
<td>
<code>peek-live-update t</code> refreshes the overlay if
                        the underlying code is modified.
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
                    Supplies the LSP payloads for definitions and references,
                    which are seamlessly consumed by <code>xref</code> and
                    subsequently rendered by <code>peek</code>.
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
<div class="eco-name">xref</div>
<div class="eco-sub">Navigation Hub</div>
</div>
</div>
<p class="eco-desc">
                    Acts as the central routing hub, ensuring that peek
                    operations share the same history ring as standard
                    <kbd>M-.</kbd> jumps, allowing <code>xref-go-back</code>
                    (<kbd>M-,</kbd>) to return to the exact pre-peek state.
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
                    Eagerly registers the <kbd>SPC c p</kbd> prefix bindings,
                    providing a consistent, mnemonic access point for all peek
                    operations without deferred-registration traps.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">evil-collection</div>
<div class="eco-sub">Modal Parity</div>
</div>
</div>
<p class="eco-desc">
                    Preserves Vim muscle memory by ensuring that standard
                    navigation keys function predictably even when an overlay is
                    active.
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
                      Built-in. Queries <code>textDocument/definition</code> or
                      <code>textDocument/references</code> and routes payloads
                      to <code>xref</code>.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref</div>
<div class="stack-role">Navigation Framework</div>
<div class="stack-desc">
                      Built-in. Resolves the target location and manages the
                      cross-referencing history.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">peek</div>
<div class="stack-role">Inline Rendering</div>
<div class="stack-desc">
                      SourceHut package. Renders the target location inline
                      below or above the cursor using native Emacs overlays,
                      avoiding tab switches or heavy child-frames.
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
<td>Peek definition</td>
<td><code>peek-xref-definition</code></td>
<td><kbd>SPC c p d</kbd></td>
<td>
                        Shows the definition inline without leaving the current
                        buffer.
                      </td>
</tr>
<tr>
<td>Peek references</td>
<td><code>peek-xref-references</code></td>
<td><kbd>SPC c p r</kbd></td>
<td>
                        Custom wrapper to show the first reference inline.
                      </td>
</tr>
<tr>
<td>Hide peek view</td>
<td><code>peek-overlay-dwim</code></td>
<td><kbd>SPC c p h</kbd></td>
<td>Toggles or hides the active peek overlay.</td>
</tr>
<tr>
<td>Standard jump (fallback)</td>
<td><code>xref-find-definitions</code></td>
<td><kbd>M-.</kbd> / <kbd>g d</kbd></td>
<td>
                        Jumps to the target in a new buffer if inline peek is
                        not desired.
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
<span class="fname">init-peek.el</span>
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
;; PEEK (Inline Overlay Engine)
;; ==========================================
(use-package peek
  :ensure (peek :host sourcehut :repo "~meow_king/peek")
  :commands (peek-xref-definition peek-xref-references peek-overlay-dwim)
  :custom
  ;; Render the peek view below the cursor to avoid obscuring the current line.
  (peek-overlay-position 'below)
  ;; Distance in lines between the cursor and the peek overlay.
  (peek-overlay-distance 2)
  ;; Number of surrounding lines to include for context.
  (peek-definition-surrounding-above-lines 1)
  ;; Automatically update the peek view if the source buffer changes.
  (peek-live-update t)
  :config
  (global-peek-mode 1)
  ;; Custom wrapper for peeking references using the same overlay engine.
  (defun peek-goto-xref-references-func (identifier)
    "Go to the first reference of IDENTIFIER and clear history."
    (xref-find-references identifier)
    (when (car (xref--get-history))
      (pop (car (xref--get-history)))))
  (defun peek-xref-references ()
    "Peek xref references inline."
    (interactive)
    (peek-definition #'peek-goto-xref-references-func (list (thing-at-point 'symbol))))
  ;; Bindings for hiding the overlay.
  (general-define-key
   :states 'normal
   "SPC c p h" #'peek-overlay-dwim))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c p" '(:ignore t :wk "peek")
  "c p d" '(peek-xref-definition :wk "Peek definition")
  "c p r" '(peek-xref-references :wk "Peek references"))</code></pre>{% endraw %}
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
<h4>✓ peek (chosen)</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Agnostic; works seamlessly with <code>eglot</code> and
                        native <code>xref</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Uses native <code>before-string</code> /
                        <code>after-string</code> overlays, preserving the
                        window tree and avoiding redisplay jitter.</span>
</div>
<div class="vs-row">
<span class="lab">Terminal Support</span>
<span class="val">Overlays render correctly in TTY environments.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-peek (rejected)</h4>
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
<span class="val">Relies on heavy child-frame pipelines that can cause
                        positioning bugs on Wayland/PGTK.</span>
</div>
<div class="vs-row">
<span class="lab">Terminal Support</span>
<span class="val">Child-frames are strictly GUI-only and fail in
                        terminals.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">
                    Refined <code>xref</code> Integration
                  </div>
<p class="enh-desc">
                    Emacs 31's mature <code>xref</code> API ensures that
                    <code>peek</code> receives accurate, AST-aware location data
                    from <code>eglot</code>, preventing the legacy regex-based
                    misidentifications that plagued older Emacs versions.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Overlay Rendering Stability</div>
<p class="enh-desc">
                    The <code>peek</code> package leverages Emacs' optimized
                    text property engine. In Emacs 31, this integrates
                    flawlessly with <code>treesit</code> fontification, ensuring
                    that the peeked code retains full syntax highlighting
                    without triggering expensive buffer-wide redisplay cycles.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">TTY Parity</div>
<p class="enh-desc">
                    Unlike child-frame-based peek implementations,
                    <code>peek</code>'s overlay approach degrades gracefully to
                    terminal emulators, ensuring inline navigation remains
                    functional over SSH or in minimal environments.
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
<td>Overlay Obscures Current Line</td>
<td>
                        Adjust <code>peek-overlay-position</code> to
                        <code>'above</code> or increase
                        <code>peek-overlay-distance</code> to push the panel
                        further away from the cursor, preventing visual
                        collision with the active editing line.
                      </td>
</tr>
<tr>
<td>Peek Shows Incorrect Location</td>
<td>
                        Ensure <code>eglot</code> is actively connected and that
                        the language server supports
                        <code>textDocument/definition</code> or
                        <code>textDocument/references</code>. Fallback to
                        <kbd>M-.</kbd> to verify if the LSP server itself is
                        returning inaccurate data.
                      </td>
</tr>
<tr>
<td>Multiple References Not Shown</td>
<td>
                        The <code>peek-xref-references</code> wrapper is
                        designed to show the first reference inline to maintain
                        the "peek" paradigm. For a comprehensive list of all
                        references, use the standard
                        <code>xref-find-references</code> (<kbd>M-?</kbd> or
                        <kbd>SPC c D</kbd>), which leverages
                        <code>consult-xref</code> for a searchable,
                        multi-candidate dropdown.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

