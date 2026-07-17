---
title: "Find All References"
category: "Navigation & Code Jumping"
status: "Working"
parity: "Shift+F12 / \"Find All References\" centralized panel with editable results"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Find All References</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Shift+F12 / "Find All References" centralized panel with editable
            results</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/references</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>xref-find-references<span class="route-arrow">→</span>consult-xref OR native *xref* buffer</code>
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
<td><kbd>Shift+F12</kbd> lists all references</td>
<td>
<kbd>M-?</kbd> or <kbd>SPC c D</kbd>
                        (<code>xref-find-references</code>)
                      </td>
</tr>
<tr>
<td>Centralized References panel</td>
<td>
<code>*xref*</code> buffer or
                        <code>consult-xref</code> vertico dropdown
                      </td>
</tr>
<tr>
<td>Click reference to jump to file</td>
<td>
<kbd>RET</kbd> on any row in the <code>*xref*</code>
                        buffer
                      </td>
</tr>
<tr>
<td>Filter references by file/path</td>
<td>
                        Type filename in <code>consult-xref</code> or use native
                        <kbd>/</kbd> filter in <code>*xref*</code>
</td>
</tr>
<tr>
<td>Edit multiple references simultaneously</td>
<td>
                        Emacs 31
                        <code>xref-change-to-xref-edit-mode</code> (<kbd>e</kbd>
                        in <code>*xref*</code> buffer)
                      </td>
</tr>
<tr>
<td><kbd>Alt+Left</kbd> returns to origin</td>
<td>
<kbd>M-,</kbd> (<code>xref-go-back</code>) via the
                        native xref history ring
                      </td>
</tr>
<tr>
<td><kbd>Ctrl+Click</kbd> on symbol</td>
<td>
                        Emacs 31 <code>global-xref-mouse-mode</code>
                        (<kbd>C-&lt;mouse-1&gt;</kbd>)
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
                    routing <kbd>M-?</kbd> to
                    <code>textDocument/references</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
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
                    reference targets.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">evil-collection</div>
<div class="eco-sub">Modal Navigation</div>
</div>
</div>
<p class="eco-desc">
                    Standardizes <kbd>[</kbd> / <kbd>]</kbd> or
                    <kbd>g</kbd> motions across all major modes, ensuring Vim
                    muscle memory is preserved when navigating the
                    <code>*xref*</code> buffer.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">apheleia</div>
<div class="eco-sub">Post-Edit Formatting</div>
</div>
</div>
<p class="eco-desc">
                    If bulk edits are made via
                    <code>xref-change-to-xref-edit-mode</code>, saving the
                    modified files automatically triggers
                    <code>apheleia</code> to format the updated code.
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
                      Built-in. Queries <code>textDocument/references</code> and
                      injects the location payloads into the native
                      <code>xref</code> framework.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
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
<div class="stack-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">consult-xref</div>
<div class="stack-role">Preview Engine</div>
<div class="stack-desc">
                      Intercepts <code>xref-show-xrefs-function</code> to render
                      a vertico-powered dropdown with live buffer previews.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref-edit-mode</div>
<div class="stack-role">Bulk Mutation</div>
<div class="stack-desc">
                      Emacs 31 NEW. Transforms the <code>*xref*</code> buffer
                      into a writable surface (Grep-Edit style) for simultaneous
                      edits.
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
<td>Find all references</td>
<td><code>xref-find-references</code></td>
<td><kbd>M-?</kbd> / <kbd>SPC c D</kbd></td>
<td>
                        Lists all usages; opens
                        <code>consult-xref</code> dropdown or
                        <code>*xref*</code> buffer.
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
<td>Edit references in place</td>
<td><code>xref-change-to-xref-edit-mode</code></td>
<td><kbd>e</kbd> (in <code>*xref*</code>)</td>
<td>
                        Emacs 31 NEW — enables writable reference buffer for
                        bulk mutation.
                      </td>
</tr>
<tr>
<td>Next reference</td>
<td><code>xref-next-line</code></td>
<td><kbd>n</kbd> (in <code>*xref*</code>)</td>
<td>Navigates down the reference list.</td>
</tr>
<tr>
<td>Previous reference</td>
<td><code>xref-prev-line</code></td>
<td><kbd>p</kbd> (in <code>*xref*</code>)</td>
<td>Navigates up the reference list.</td>
</tr>
<tr>
<td>Filter references</td>
<td><code>consult-xref</code></td>
<td><kbd>SPC c E</kbd></td>
<td>
                        Fuzzy-filters the reference list with live preview.
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
<span class="fname">init-references.el</span>
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
  ;; Route xref location prompts through Consult for live previews.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  (global-xref-mouse-mode 1))

;; ==========================================
;; 2. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(general-define-key
  :states 'motion
  "g D" #'xref-find-references  ;; Go to References (Shift+F12 parity)
  "g ," #'xref-go-back)         ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c D" '(xref-find-references :wk "Find all references"))</code></pre>{% endraw %}
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
<span class="lab">Bulk Mutation</span>
<span class="val">Emacs 31 native
                        <code>xref-change-to-xref-edit-mode</code> (Grep-Edit
                        parity).</span>
</div>
<div class="vs-row">
<span class="lab">Preview</span>
<span class="val"><code>consult-xref</code> leverages
                        <code>vertico</code> for fuzzy filtering and live
                        previews.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-references / lsp-mode · Rejected</h4>
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
<span class="lab">Bulk Mutation</span>
<span class="val">Requires fragile third-party wrappers or manual text
                        replacement.</span>
</div>
<div class="vs-row">
<span class="lab">Preview</span>
<span class="val">Custom child-frame pipeline with heavy rendering
                        overhead.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
                    xref-change-to-xref-edit-mode (NEW)
                  </div>
<p class="enh-desc">
                    Bound to <kbd>e</kbd> inside the <code>*xref*</code> buffer,
                    it transforms the read-only list into a writable surface.
                    Perform bulk text replacements across all listed references
                    simultaneously. Upon saving, edits propagate natively back
                    to the originating source files.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">xref-mouse-mode (NEW)</div>
<p class="enh-desc">
                    Emacs 31 introduces native mouse-driven code navigation.
                    Enabling <code>global-xref-mouse-mode</code> binds
                    <kbd>C-&lt;down-mouse-1&gt;</kbd> to xref jumps, perfectly
                    mirroring VS Code's Ctrl+Click convention.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">consult-xref synergy</div>
<p class="enh-desc">
                    Ambiguous references are presented in a highly performant,
                    searchable Vertico dropdown with instant buffer previews,
                    eliminating the need to cycle through blind
                    <code>*xref*</code> buffer splits.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Unified xref history</div>
<p class="enh-desc">
<code>xref-go-back</code> (<kbd>M-,</kbd>) treats reference
                    jumps identically to definition and implementation jumps —
                    the entire navigation chain is preserved in a single ring.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

