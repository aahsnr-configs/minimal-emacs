---
title: "Inlay Hints"
category: "Completion & Intelligence"
status: "Working"
parity: "Inline grey annotations showing inferred types, parameter names, etc."
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Inlay Hints</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Inline grey annotations showing inferred types, parameter names,
            etc.</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/inlayHint</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>inlayHint/resolve</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-inlay-hints-mode<span class="route-arrow">→</span>native
              buffer overlays</code>
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
<td>Inline grey text for parameter names</td>
<td>
<code>eglot-inlay-hint-parameter-face</code> renders
                        recessive, italicized parameter names.
                      </td>
</tr>
<tr>
<td>Inline type annotations for variables</td>
<td>
<code>eglot-inlay-hint-type-face</code> renders inferred
                        types adjacent to declarations.
                      </td>
</tr>
<tr>
<td>Hints disappear when typing in the hint location</td>
<td>
<code>eglot</code> automatically clears and re-requests
                        hints on buffer modification.
                      </td>
</tr>
<tr>
<td>Click/hover to see full resolved hint</td>
<td>
<code>eglot</code> natively triggers
                        <code>inlayHint/resolve</code> when the cursor rests on
                        or interacts with the hint overlay.
                      </td>
</tr>
<tr>
<td>Toggle hints via command palette</td>
<td>
<kbd>SPC t h</kbd> (<code>eglot-inlay-hints-mode</code>)
                        or <kbd>M-x global-eglot-inlay-hints-mode</kbd>.
                      </td>
</tr>
<tr>
<td>Respects variable-pitch fonts</td>
<td>
                        Emacs 31's overlay renderer correctly calculates spacing
                        even when mixing monospaced code with variable-pitch
                        hint text.
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
                    Natively handles the entire lifecycle of inlay hints, from
                    capability negotiation during initialization to overlay
                    cleanup on buffer kill.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">Font-lock Priority</div>
</div>
</div>
<p class="eco-desc">
                    Inlay hint overlays are applied with a lower priority than
                    <code>treesit</code> font-lock, ensuring that primary syntax
                    highlighting always takes visual precedence.
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
<div class="eco-name">doom-themes</div>
<div class="eco-sub">Visual Styling</div>
</div>
</div>
<p class="eco-desc">
                    The custom face definitions seamlessly inherit the Tokyo
                    Night <code>shadow</code> and specific accent colors,
                    maintaining a cohesive, professional IDE aesthetic.
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
                    The <kbd>SPC t h</kbd> keybinding provides a consistent,
                    mnemonic toggle for inlay hints across all programming
                    buffers, aligning with Doom Emacs muscle memory.
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
                      Built-in. Negotiates <code>inlayHintProvider</code>
                      capabilities and requests hint payloads via
                      <code>textDocument/inlayHint</code> and
                      <code>inlayHint/resolve</code>.
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
<div class="stack-name">eglot-inlay-hints-mode</div>
<div class="stack-role">Rendering Engine</div>
<div class="stack-desc">
                      Built-in minor mode that applies hint text as
                      <code>before-string</code> or <code>after-string</code>
                      overlays directly in the buffer.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-inlay-hint-* faces</div>
<div class="stack-role">Visual Styling</div>
<div class="stack-desc">
                      Dedicated faces for different hint kinds (e.g.,
                      <code>eglot-inlay-hint-type-face</code>) allowing precise
                      theme integration.
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
<td>Toggle inlay hints</td>
<td><code>eglot-inlay-hints-mode</code></td>
<td><kbd>SPC t h</kbd></td>
<td>
                        Enables/disables inline annotations for the current
                        buffer.
                      </td>
</tr>
<tr>
<td>Toggle globally</td>
<td><code>global-eglot-inlay-hints-mode</code></td>
<td>—</td>
<td>
                        Enables inlay hints across all
                        <code>eglot</code>-managed buffers.
                      </td>
</tr>
<tr>
<td>Resolve hint details</td>
<td><code>eglot-inlay-hint-resolve</code></td>
<td>—</td>
<td>
                        Triggered automatically by <code>eglot</code> when
                        hovering or interacting with a hint.
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
<span class="fname">init-inlay-hints.el</span>
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
;; EGLOT INLAY HINTS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :hook ((prog-mode . eglot-ensure))
  :config
  ;; Enable inlay hints globally for all eglot-managed buffers.
  ;; Can be toggled per-buffer via `eglot-inlay-hints-mode' or `SPC t h`.
  (global-eglot-inlay-hints-mode 1)
  ;; ==========================================
  ;; VISUAL STYLING (Tokyo Night Synergy)
  ;; ==========================================
  ;; Inlay hints should be recessive to avoid competing with primary syntax highlighting.
  (custom-set-faces
   '(eglot-inlay-hint-face ((t (:inherit shadow :height 0.9 :slant italic))))
   '(eglot-inlay-hint-type-face ((t (:inherit shadow :foreground "#73daca" :height 0.9 :slant italic))))
   '(eglot-inlay-hint-parameter-face ((t (:inherit shadow :foreground "#bb9af7" :height 0.9 :slant italic))))))</code></pre>{% endraw %}
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
<span class="val">Works exclusively with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Rendering Physics</span>
<span class="val">Uses native Emacs <code>before-string</code> /
                        <code>after-string</code> text properties, ensuring
                        seamless integration with <code>treesit</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Lightweight overlay application; defers
                        <code>inlayHint/resolve</code> network calls until
                        explicitly needed.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / lsp-ui · Rejected</h4>
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
<span class="lab">Rendering Physics</span>
<span class="val">Historically relied on complex, fragile overlay
                        management that could conflict with native
                        font-lock.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Aggressive background resolution can cause main-thread
                        micro-stutters on slower servers.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Variable-Pitch Font Support</div>
<p class="enh-desc">
                    Emacs 31's <code>eglot</code> overlay renderer has been
                    explicitly optimized to calculate bounding boxes and spacing
                    correctly when <code>variable-pitch-mode</code> is active,
                    preventing misaligned hint text.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Granular Face Customization</div>
<p class="enh-desc">
                    Upstream <code>eglot</code> now exposes distinct faces for
                    different hint kinds
                    (<code>eglot-inlay-hint-type-face</code>,
                    <code>eglot-inlay-hint-parameter-face</code>), allowing
                    users to color-code hints without resorting to fragile
                    regex-based font-lock hacks.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Efficient inlayHint/resolve</div>
<p class="enh-desc">
<code>eglot</code> intelligently batches and debounces
                    <code>inlayHint/resolve</code> requests, ensuring that
                    hovering over or interacting with a hint does not spam the
                    language server, preserving the 60fps typing experience.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

