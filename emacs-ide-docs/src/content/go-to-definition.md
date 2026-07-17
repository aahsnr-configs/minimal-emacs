---
title: "Go to Definition"
category: "Navigation & Code Jumping"
status: "Working"
parity: "F12 to jump, Ctrl+Click to jump, Alt+F12 to peek inline"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Go to Definition</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>F12 to jump, Ctrl+Click to jump, Alt+F12 to peek inline</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/definition</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>xref<span class="route-arrow">→</span>consult-xref OR peek</code>
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
<td><kbd>F12</kbd> jumps to definition</td>
<td>
<kbd>M-.</kbd> or <kbd>g d</kbd>
                        (<code>xref-find-definitions</code>)
                      </td>
</tr>
<tr>
<td>Ctrl+Click jumps to definition</td>
<td>
<kbd>C-&lt;mouse-1&gt;</kbd> via Emacs 31
                        <code>global-xref-mouse-mode</code>
</td>
</tr>
<tr>
<td>Alt+F12 peeks definition inline</td>
<td>
<kbd>SPC c p d</kbd> (<code>peek-xref-definition</code>)
                        via <code>peek</code> package
                      </td>
</tr>
<tr>
<td>Dropdown for multiple definitions</td>
<td>
<code>consult-xref</code> intercepts
                        <code>xref-show-definitions-function</code>
</td>
</tr>
<tr>
<td>Live preview in dropdown</td>
<td>
<code>consult-xref</code> + <code>vertico</code> live
                        buffer previews
                      </td>
</tr>
<tr>
<td>Alt+Left returns to origin</td>
<td>
<kbd>M-,</kbd> or <kbd>g ,</kbd>
                        (<code>xref-go-back</code>)
                      </td>
</tr>
<tr>
<td>Open in split window</td>
<td>
<kbd>C-x 4 .</kbd>
                        (<code>xref-find-definitions-other-window</code>)
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
                    Automatically registers <code>eglot-xref-backend</code> in
                    <code>xref-backend-functions</code> for managed buffers,
                    routing <kbd>M-.</kbd> to
                    <code>textDocument/definition</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div>
<div class="eco-name">consult</div>
<div class="eco-sub">Preview Engine</div>
</div>
</div>
<p class="eco-desc">
<code>consult-xref</code> intercepts the xref display
                    functions to provide vertico-powered previews for ambiguous
                    definition targets.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
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
                    For non-LSP buffers (or when eglot is disconnected),
                    <code>treesit</code> modes provide fallback definition
                    jumping via <code>treesit-thing</code> navigation or
                    <code>imenu</code> integration.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">peek</div>
<div class="eco-sub">Inline Overlay</div>
</div>
</div>
<p class="eco-desc">
                    Provides the inline overlay engine for "Peek Definition"
                    workflows, keeping the user's spatial context intact without
                    switching tabs.
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
                      Built-in. Drives <code>textDocument/definition</code> and
                      injects the location payload into the native
                      <code>xref</code> framework.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref</div>
<div class="stack-role">Navigation Framework</div>
<div class="stack-desc">
                      Built-in. Manages location abstraction, shared history
                      ring, and cross-buffer jumping.
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
<div class="stack-name">consult-xref</div>
<div class="stack-role">Preview Engine</div>
<div class="stack-desc">
                      Intercepts <code>xref-show-definitions-function</code> to
                      render a vertico-powered dropdown with live buffer
                      previews.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">peek</div>
<div class="stack-role">Inline Peek</div>
<div class="stack-desc">
                      Renders the definition inline below the cursor using
                      overlays without switching tabs (Alt+F12 parity).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(224, 175, 104, 0.1);
                      color: var(--yellow);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref-mouse-mode</div>
<div class="stack-role">Mouse Integration</div>
<div class="stack-desc">
                      Emacs 31 NEW. Binds <kbd>C-&lt;mouse-1&gt;</kbd> to
                      <code>xref-find-definitions-at-mouse</code>, enabling
                      native Ctrl+Click jumps.
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
<td>Jump to definition</td>
<td><code>xref-find-definitions</code></td>
<td><kbd>M-.</kbd> / <kbd>g d</kbd></td>
<td>
                        Jumps to target; opens
                        <code>consult-xref</code> dropdown if ambiguous.
                      </td>
</tr>
<tr>
<td>Jump to definition (mouse)</td>
<td><code>xref-find-definitions-at-mouse</code></td>
<td><kbd>C-&lt;mouse-1&gt;</kbd></td>
<td>
                        Emacs 31 NEW — Ctrl+Click parity via
                        <code>global-xref-mouse-mode</code>.
                      </td>
</tr>
<tr>
<td>Peek definition (inline)</td>
<td><code>peek-xref-definition</code></td>
<td><kbd>SPC c p d</kbd></td>
<td>
                        Shows target in an inline overlay panel (requires
                        <code>peek</code> package).
                      </td>
</tr>
<tr>
<td>Go back (history)</td>
<td><code>xref-go-back</code></td>
<td><kbd>M-,</kbd> / <kbd>g ,</kbd></td>
<td>
                        Returns to the exact cursor position before the jump.
                      </td>
</tr>
<tr>
<td>Go forward (history)</td>
<td><code>xref-go-forward</code></td>
<td>—</td>
<td>Reverses <code>xref-go-back</code>.</td>
</tr>
<tr>
<td>Find definitions (other window)</td>
<td><code>xref-find-definitions-other-window</code></td>
<td><kbd>C-x 4 .</kbd></td>
<td>Opens definition in a horizontal split.</td>
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
<span class="fname">init-xref.el</span>
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
;; 1. XREF &amp; CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  ;; Binds `C-&lt;down-mouse-1&gt;` to `xref-find-definitions-at-mouse`.
  :config
  (global-xref-mouse-mode 1)
  ;; Route xref location prompts through Consult for live previews.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; ==========================================
;; 2. PEEK PACKAGE (Inline Definition Panel)
;; ==========================================
(use-package peek
  :ensure (peek :host sourcehut :repo "~meow_king/peek")
  :commands (peek-xref-definition peek-overlay-dwim)
  :custom
  (peek-mode-enable-eldoc t)        ;; show eldoc inside peek panel
  (peek-definition-function #'xref-find-definitions)
  :config
  (global-peek-mode 1))

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(general-define-key
  :states 'motion
  "g d" #'xref-find-definitions
  "g ," #'xref-go-back)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c p" '(:ignore t :wk "peek")
  "c p d" '(peek-xref-definition :wk "Peek definition"))</code></pre>{% endraw %}
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
<h4>✓ xref + consult-xref · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Works with <i>any</i> xref backend (eglot, dumb-jump,
                        etags).</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Preview</span>
<span class="val"><code>consult-xref</code> leverages
                        <code>vertico</code> for fuzzy filtering and live
                        previews.</span>
</div>
<div class="vs-row">
<span class="lab">Mouse</span>
<span class="val">Native Emacs 31 <code>xref-mouse-mode</code> (zero
                        dependencies).</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-peek / lsp-mode · Rejected</h4>
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
<span class="lab">Preview</span>
<span class="val">Custom child-frame pipeline with heavy rendering
                        overhead.</span>
</div>
<div class="vs-row">
<span class="lab">Mouse</span>
<span class="val">Requires custom mouse-click advice.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">xref-mouse-mode (NEW)</div>
<p class="enh-desc">
                    Emacs 31 introduces native mouse-driven code navigation.
                    Enabling <code>global-xref-mouse-mode</code> binds
                    <kbd>C-&lt;down-mouse-1&gt;</kbd> to xref jumps, perfectly
                    mirroring VS Code's Ctrl+Click convention.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Editable Xref Buffers</div>
<p class="enh-desc">
                    Emacs 31's <code>xref-change-to-xref-edit-mode</code> (bound
                    to <kbd>e</kbd> in <code>*xref*</code> buffers) turns
                    reference lists into writable surfaces for bulk mutation.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">consult-xref synergy</div>
<p class="enh-desc">
                    Ambiguous definitions (e.g., C++ overloaded functions,
                    TypeScript union types) are presented in a highly
                    performant, searchable Vertico dropdown with instant buffer
                    previews.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">peek package integration</div>
<p class="enh-desc">
                    The <code>peek</code> package hooks directly into the
                    <code>xref</code> framework via
                    <code>peek-definition-function</code>, allowing it to
                    intercept payloads and render them as inline overlays.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

