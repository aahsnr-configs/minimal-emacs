---
title: "Hover Info"
category: "Completion & Intelligence"
status: "Working"
parity: "Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Hover Info</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Tooltip with type info, docs, and signatures on mouse-hover or
            keyboard shortcut</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/hover</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eldoc<span class="route-arrow">→</span>ephemeral buffer</code>
</div>
</div>
</header>

<article class="acc">
<button aria-controls="hover-sect-overview" aria-expanded="true" class="acc-head open">
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
<div class="acc-body open" id="hover-sect-overview" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Behavioral Parity Matrix</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>VS Code Behavior</th>
<th>Emacs Equivalent</th>
</tr>
</thead>
<tbody>
<tr>
<td>Hover tooltip on cursor idle</td>
<td>
<code>eldoc</code> with
                        <code>eldoc-idle-delay 0.5</code> triggers
                        <code>textDocument/hover</code>
</td>
</tr>
<tr>
<td>Mouse hover shows tooltip</td>
<td>
<code>eldoc-help-at-pt t</code> surfaces info on cursor
                        movement
                      </td>
</tr>
<tr>
<td><kbd>K</kbd> key shows hover (Vim)</td>
<td>
<kbd>K</kbd> bound to <code>eldoc</code> in Evil normal
                        state
                      </td>
</tr>
<tr>
<td>Rich markdown rendering</td>
<td>
<code>markdown-ts-mode</code> fontifies
                        <code>*eldoc*</code> ephemeral buffer
                      </td>
</tr>
<tr>
<td>Long docs in side panel</td>
<td>
<code>eldoc-echo-area-prefer-doc-buffer t</code>
                        routes to <code>*eldoc*</code> buffer
                      </td>
</tr>
<tr>
<td>Scroll long documentation</td>
<td>
<kbd>C-M-v</kbd> / <kbd>C-M-S-v</kbd> scroll the
                        <code>*eldoc*</code> buffer
                      </td>
</tr>
<tr>
<td>Hover on completion candidate</td>
<td>
<code>corfu-popupinfo-toggle</code> (<kbd>M-h</kbd>)
                        shows <code>completionItem/resolve</code> docs
                      </td>
</tr>
<tr>
<td>Signature help in tooltip</td>
<td>
<code>eldoc</code> natively merges
                        <code>textDocument/signatureHelp</code> with hover
                      </td>
</tr>
<tr>
<td>Type info + docstring combined</td>
<td>
<code>eglot</code> merges both payloads into single
                        eldoc response
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
<button aria-controls="hover-sect-ecosystem" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="hover-sect-ecosystem" role="region">
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
                    Automatically injects
                    <code>eglot-hover-eldoc-function</code> into
                    <code>eldoc-documentation-functions</code> when a buffer is
                    LSP-managed.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">eldoc</div>
<div class="eco-sub">Documentation Router</div>
</div>
</div>
<p class="eco-desc">
                    Serves as the unified router — aggregating LSP hover, Elisp
                    docstrings, and <code>help-at-point</code> into a single
                    echo-area / ephemeral-buffer pipeline.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">corfu-popupinfo</div>
<div class="eco-sub">Candidate Hover</div>
</div>
</div>
<p class="eco-desc">
                    Handles completion-candidate-level documentation (via
                    <code>completionItem/resolve</code>), keeping candidate
                    hover separate from symbol hover.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">Markdown Fontification</div>
</div>
</div>
<p class="eco-desc">
                    Enables <code>markdown-ts-mode</code> fontification inside
                    the <code>*eldoc*</code> buffer, providing
                    syntax-highlighted code blocks in hover documentation.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(224, 175, 104, 0.1); color: var(--yellow);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div>
<div class="eco-name">which-key</div>
<div class="eco-sub">Priority Management</div>
</div>
</div>
<p class="eco-desc">
                    Echo-area priority is preserved —
                    <code>eldoc</code> yields to <code>which-key</code> popups
                    to prevent documentation from clobbering keybinding hints.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="hover-sect-stack" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="hover-sect-stack" role="region">
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
                      Built-in. Drives <code>textDocument/hover</code>, returns
                      Markdown payloads natively.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eldoc</div>
<div class="stack-role">Documentation Engine</div>
<div class="stack-desc">
                      Built-in. Renders hover payloads in the echo area or
                      ephemeral <code>*eldoc*</code> buffer.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">markdown-ts-mode</div>
<div class="stack-role">Markdown Rendering</div>
<div class="stack-desc">
                      Built-in. Provides rich tree-sitter markdown fontification
                      inside the ephemeral buffer.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">corfu-popupinfo</div>
<div class="stack-role">Candidate-Level Hover</div>
<div class="stack-desc">
                      Bundled with corfu. Shows
                      <code>completionItem/resolve</code> docs adjacent to the
                      completion popup.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(224, 175, 104, 0.1); color: var(--yellow);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 16v-4M12 8h.01"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">help-at-point-kbd-string</div>
<div class="stack-role">Mouse Integration</div>
<div class="stack-desc">
                      Built-in. Surfaces hover info on cursor hover via
                      <code>eldoc-help-at-pt</code>.
                    </div>
</div>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="hover-sect-commands" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="hover-sect-commands" role="region">
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
<td>Hover at point (keyboard)</td>
<td><code>eldoc</code></td>
<td><kbd>K</kbd> (Evil normal)</td>
<td>
                        Shows type info / docstring for symbol under cursor.
                      </td>
</tr>
<tr>
<td>Help at point</td>
<td><code>help-at-point</code></td>
<td><kbd>C-h .</kbd></td>
<td>
                        Native Emacs help surfacing via
                        <code>eldoc-help-at-pt</code>.
                      </td>
</tr>
<tr>
<td>Toggle candidate docs</td>
<td><code>corfu-popupinfo-toggle</code></td>
<td><kbd>M-h</kbd> (in <code>corfu-map</code>)</td>
<td>
                        Shows/hides doc popup for selected completion candidate.
                      </td>
</tr>
<tr>
<td>Scroll doc buffer</td>
<td><code>scroll-other-window</code></td>
<td><kbd>C-M-v</kbd></td>
<td>
                        Scrolls the <code>*eldoc*</code> ephemeral buffer when
                        doc exceeds echo area.
                      </td>
</tr>
<tr>
<td>Scroll doc buffer (back)</td>
<td><code>scroll-other-window-down</code></td>
<td><kbd>C-M-S-v</kbd></td>
<td>Reverse scroll for long documentation.</td>
</tr>
<tr>
<td>Force hover refresh</td>
<td><code>eglot-signature-eldoc-bar</code></td>
<td>—</td>
<td>
                        Re-queries <code>textDocument/hover</code> on demand.
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
<button aria-controls="hover-sect-config" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="hover-sect-config" role="region">
<div>
<div class="acc-inner">
<div class="code-win">
<div class="code-head">
<div style="display: flex; align-items: center">
<div aria-hidden="true" class="dots">
<span></span><span></span><span></span>
</div>
<span class="fname">init-hover.el</span>
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
;; 2. ELDOC CORE (Emacs Native Rendering)
;; ==========================================
(use-package eldoc
:ensure nil
:custom
;; Surface `help-at-point-kbd-string` through the eldoc pipeline.
(eldoc-help-at-pt t)
;; Prefer the ephemeral `*eldoc*` buffer over the echo area
;; when documentation exceeds a single line.
(eldoc-echo-area-prefer-doc-buffer t)
(eldoc-echo-area-use-multiline-p t)
(eldoc-idle-delay 0.5)
:config
(global-eldoc-mode 1))
;; ==========================================
;; 3. CORFU-POPUPINFO (Candidate-Level Hover)
;; ==========================================
(use-package corfu-popupinfo
:ensure nil
:after corfu
:bind (:map corfu-map
("M-h" . corfu-popupinfo-toggle))
:config
(corfu-popupinfo-mode 1)
;; Documentation is ONLY shown on manual trigger (M-h).
(setq corfu-popupinfo-delay nil))
;; ==========================================
;; 4. GENERAL.EL KEYBINDINGS
;; ==========================================
(general-define-key
:states 'normal
"K" #'eldoc)</code></pre>{% endraw %}
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="hover-sect-arch" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="hover-sect-arch" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Why This Approach?</div>
<div class="grid-2" style="margin-bottom: 24px;">
<div class="vs-card ok">
<h4>✓ eldoc native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span><span class="val">Works with <i>any</i> eldoc backend (eglot, native
                        elisp).</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span><span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Integration</span><span class="val">Native ephemeral buffer with
                        <code>markdown-ts-mode</code> rendering.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span><span class="val">Zero additional packages, echo-area fast path.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-doc / eldoc-box · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span><span class="val">Hard-bound to <code>lsp-mode</code> or requires
                        child-frame overhead.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span><span class="val">Requires forbidden <code>lsp-mode</code> ecosystem or
                        third-party dependencies.</span>
</div>
<div class="vs-row">
<span class="lab">Integration</span><span class="val">No integration with Emacs native eldoc
                        enhancements.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span><span class="val">Child-frame latency on every hover / rendering
                        overhead.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">eldoc-help-at-pt (NEW)</div>
<p class="enh-desc">
                    Surfaces <code>help-at-point-kbd-string</code> through the
                    eldoc pipeline, enabling hover-style information on cursor
                    movement without third-party packages.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">
                    eldoc-echo-area-prefer-doc-buffer (NEW)
                  </div>
<p class="enh-desc">
                    When documentation exceeds the echo area, Emacs 31
                    automatically routes the payload to an ephemeral
                    <code>*eldoc*</code> buffer rendered with
                    <code>markdown-ts-mode</code>.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">markdown-ts-mode Integration</div>
<p class="enh-desc">
                    Emacs 31's native tree-sitter markdown mode fontifies the
                    ephemeral <code>*eldoc*</code> buffer, providing rich code
                    blocks and syntax highlighting inside hover documentation.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">TTY-Safe Rendering</div>
<p class="enh-desc">
                    Unlike child-frame-based solutions (<code>eldoc-box</code>,
                    <code>lsp-ui-doc</code>), the ephemeral buffer approach
                    degrades gracefully to TTY frames, ensuring documentation is
                    accessible over SSH.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

