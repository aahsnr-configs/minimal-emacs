---
title: "Semantic Tokens (Semantic Highlighting)"
category: "Completion & Intelligence"
status: "Working"
parity: "Type-aware syntax coloring beyond what static TextMate grammars can do"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Semantic Tokens (Semantic Highlighting)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Type-aware syntax coloring beyond what static TextMate grammars can
            do</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/semanticTokens/full</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>range</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>delta</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-semantic-tokens-mode<span class="route-arrow">→</span>native font-lock / treesit</code>
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
<td>Type-aware syntax coloring</td>
<td>
<code>eglot-semantic-tokens-mode</code> applies faces
                        based on LSP token types (e.g., distinguishing a
                        <code>parameter</code> from a
                        <code>local variable</code>).
                      </td>
</tr>
<tr>
<td>Delta updates on edit</td>
<td>
                        Eglot natively requests
                        <code>textDocument/semanticTokens/delta</code> to
                        minimize network payload and main-thread blocking.
                      </td>
</tr>
<tr>
<td>Range requests on scroll</td>
<td>
                        Eglot requests
                        <code>textDocument/semanticTokens/range</code> for
                        visible regions only, optimizing performance in massive
                        files.
                      </td>
</tr>
<tr>
<td>Customizable token colors</td>
<td>
<code>eglot-semantic-faces</code> customization group
                        allows mapping specific LSP modifiers (e.g.,
                        <code>readonly</code>, <code>deprecated</code>) to Tokyo
                        Night palette faces.
                      </td>
</tr>
<tr>
<td>Fallback to structural highlighting</td>
<td>
                        If the LSP server is slow or disconnects,
                        <code>treesit</code> (level 4) maintains perfect,
                        zero-latency structural syntax highlighting.
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
                    Natively handles the negotiation of
                    <code>semanticTokensProvider</code> capabilities during the
                    LSP initialization handshake.
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
<div class="eco-sub">Baseline Highlighting</div>
</div>
</div>
<p class="eco-desc">
                    Provides the foundational
                    <code>treesit-font-lock-level 4</code> highlighting,
                    ensuring that even if semantic tokens are disabled, the
                    buffer remains beautifully and accurately highlighted.
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
                    The Tokyo Night theme can be extended to map specific
                    <code>eglot-semantic-*</code> faces to the palette's neon
                    accents, creating a cohesive, type-aware visual experience.
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
<div class="eco-name">apheleia</div>
<div class="eco-sub">Formatting Synergy</div>
</div>
</div>
<p class="eco-desc">
                    Formatting operations do not disrupt semantic token
                    overlays, as Eglot efficiently recalculates token positions
                    post-edit via delta requests.
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
                      Built-in. Negotiates
                      <code>semanticTokensProvider</code> capabilities and
                      requests token payloads via full, range, or delta methods.
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
<div class="stack-name">eglot-semantic-tokens-mode</div>
<div class="stack-role">Semantic Engine</div>
<div class="stack-desc">
                      Built-in Eglot minor mode that applies LSP semantic token
                      types and modifiers to the buffer using text properties.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">treesit</div>
<div class="stack-role">Baseline Highlighting</div>
<div class="stack-desc">
                      Built-in. Provides fast, C-level structural syntax
                      highlighting. Semantic tokens augment this baseline rather
                      than replacing it.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-semantic-faces</div>
<div class="stack-role">Face Customization</div>
<div class="stack-desc">
                      Customization group allowing users to map specific LSP
                      token types (e.g., <code>variable.readonly</code>) to
                      Emacs faces.
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
<td>Toggle semantic tokens</td>
<td><code>eglot-semantic-tokens-mode</code></td>
<td><kbd>SPC t s</kbd></td>
<td>
                        Enables/disables LSP semantic highlighting for the
                        current buffer.
                      </td>
</tr>
<tr>
<td>Customize token faces</td>
<td><code>customize-group</code></td>
<td>
<kbd>M-x customize-group RET eglot-semantic-faces</kbd>
</td>
<td>
                        Adjust colors for specific token types (e.g.,
                        parameters, macros).
                      </td>
</tr>
<tr>
<td>Toggle inlay hints (companion)</td>
<td><code>eglot-inlay-hints-mode</code></td>
<td><kbd>SPC t h</kbd></td>
<td>
                        Often used alongside semantic tokens for full type-aware
                        annotation.
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
<span class="fname">init-semantic-tokens.el</span>
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
;; EGLOT SEMANTIC TOKENS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :hook ((prog-mode . eglot-ensure))
  :config
  ;; Semantic tokens are enabled by default in modern Eglot if the server supports them.
  ;; We explicitly ensure the mode is active and configure it to augment, not replace, treesit.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Enable semantic tokens for enhanced type-aware highlighting
              (eglot-semantic-tokens-mode 1)))
  ;; Optional: Fine-tune which token types/modifiers are considered for performance.
  ;; By default, Eglot respects the server's legend, but you can filter if needed.
  ;; (setq eglot-semantic-token-types '(variable parameter function method))
  )

;; ==========================================
;; TREESIT BASELINE (Emacs 31 Native)
;; ==========================================
;; Ensure treesit provides the foundational structural highlighting.
;; Semantic tokens will layer on top of this for type-specific nuances.
(setq treesit-font-lock-level 4) ;; Maximum structural decoration</code></pre>
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
<span class="lab">Performance</span>
<span class="val">Leverages Emacs 31's optimized text property
                        application; defers to <code>treesit</code> for
                        baseline.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">Natively integrates with
                        <code>treesit-font-lock-level 4</code>, allowing LSP to
                        augment structural highlighting.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode semantic highlighting · Rejected</h4>
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
<span class="val">Historically heavy; applies full buffer fontification
                        independently, causing micro-stutters.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">Often overrides or conflicts with native tree-sitter
                        fontification rules.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">eglot-semantic-tokens-mode (NEW)</div>
<p class="enh-desc">
                    Officially integrated into Eglot, this minor mode provides
                    enhanced syntax highlighting based on the language server's
                    semantic analysis, going beyond traditional
                    regular-expression-based fontification.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">treesit Augmentation</div>
<p class="enh-desc">
                    Emacs 31's <code>treesit</code> engine provides a robust,
                    C-level baseline. Eglot's semantic tokens are designed to
                    augment this baseline, applying specific faces without
                    stripping the underlying structural tree-sitter highlights.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Delta &amp; Range Optimization</div>
<p class="enh-desc">
                    Modern Eglot implementations efficiently handle
                    <code>semanticTokens/delta</code> and
                    <code>semanticTokens/range</code> requests, ensuring that
                    typing or scrolling in large files does not trigger
                    full-buffer re-highlighting network requests.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Face Customization Group</div>
<p class="enh-desc">
                    The <code>eglot-semantic-faces</code> group allows precise
                    control over how token types (e.g., <code>namespace</code>,
                    <code>type</code>) and modifiers (e.g.,
                    <code>declaration</code>, <code>readonly</code>) are
                    rendered, enabling perfect alignment with the Tokyo Night
                    theme.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

